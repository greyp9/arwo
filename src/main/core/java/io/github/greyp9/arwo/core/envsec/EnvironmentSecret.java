package io.github.greyp9.arwo.core.envsec;

import io.github.greyp9.arwo.core.charset.UTF8Codec;
import io.github.greyp9.arwo.core.codec.b64.Base64Codec;
import io.github.greyp9.arwo.core.envsec.eval.Evaluator;
import io.github.greyp9.arwo.core.envsec.eval.EvaluatorRegistry;
import io.github.greyp9.arwo.core.expr.Grammar;
import io.github.greyp9.arwo.core.expr.Node;
import io.github.greyp9.arwo.core.expr.Operand;
import io.github.greyp9.arwo.core.expr.Tree;
import io.github.greyp9.arwo.core.expr.op.MultiOperator;
import io.github.greyp9.arwo.core.ff.DataSplitter;
import io.github.greyp9.arwo.core.hash.secure.HashU;
import io.github.greyp9.arwo.core.io.ByteU;
import io.github.greyp9.arwo.core.io.StreamU;
import io.github.greyp9.arwo.core.jce.KeyCodec;
import io.github.greyp9.arwo.core.jce.KeyU;
import io.github.greyp9.arwo.core.jce.KeyX;
import io.github.greyp9.arwo.core.value.NameTypeValue;
import io.github.greyp9.arwo.core.value.Value;
import io.github.greyp9.arwo.core.xml.DocumentU;
import io.github.greyp9.arwo.core.xml.ElementU;
import org.w3c.dom.Document;
import org.w3c.dom.Element;

import javax.crypto.SecretKey;
import java.io.File;
import java.io.IOException;
import java.security.GeneralSecurityException;
import java.security.KeyException;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;
import java.util.Optional;
import java.util.Random;
import java.util.logging.Logger;

public final class EnvironmentSecret {
    private final Logger logger = Logger.getLogger(getClass().getName());

    private final String resourceExpression;
    private final String resourceShares;
    private final Random random;
    private final EvaluatorRegistry registry;

    public EnvironmentSecret(final String resource, final Random random) {
        this.resourceExpression = resource;
        this.resourceShares = resource + ".xml";
        this.random = random;
        this.registry = new EvaluatorRegistry();
    }

    public void register(final String key, final Evaluator evaluator) throws IOException {
        registry.register(key, evaluator);
    }

    public void generate(final byte[] secret) throws IOException, GeneralSecurityException {
        final File fileExpression = new File(resourceExpression);
        final String expression = UTF8Codec.toString(StreamU.read(fileExpression));
        final Grammar grammar = new Grammar(expression);
        final Tree tree = new Tree(grammar.toNode());
        final MultiOperator operatorSecret = Optional.of(tree.getRoot())
                .map(MultiOperator.class::cast)
                .filter(m -> m.getOp().equals(Const.SECRET))
                .orElseThrow(IOException::new);
        final Element elementSecret = generateA(operatorSecret, secret);
        ElementU.setAttribute(elementSecret, Const.HASH, toHash(expression, secret));
        final byte[] xml = DocumentU.toXml(elementSecret);
        logger.finest(UTF8Codec.toString(xml));
        final File fileShares = new File(resourceShares);
        StreamU.write(fileShares, xml);
    }

    private Element generateA(final MultiOperator operator, final byte[] data)
            throws GeneralSecurityException, IOException {
        final List<Node> operands = new ArrayList<>(operator.getOperands());
        final Operand operandThresholdN = (Operand) operands.remove(0);
        final int sharesN = operands.size();
        final int thresholdN = Integer.parseInt(operandThresholdN.getValue());
        final DataSplitter dataSplitter = new DataSplitter(sharesN, thresholdN, random);
        final byte[][] shares = dataSplitter.split(data);
        // assemble store of share data
        final Document document = DocumentU.createDocument(Const.SECRET, Const.URI);
        final Element element = document.getDocumentElement();
        for (int i = 0; (i < operands.size()); ++i) {
            final MultiOperator operatorChild = (MultiOperator) operands.get(i);
            if (Const.SECRET.equals(operatorChild.getOp())) {
                final Element elementChild = generateA(operatorChild, shares[i]);
                element.getOwnerDocument().adoptNode(elementChild);
                ElementU.addElement(element, elementChild, null);
            } else {
                final NameTypeValue nameTypeValue = registry.evaluate(operatorChild);
                final byte[] share = protect(nameTypeValue, shares[i]);
                ElementU.addElement(element, Const.SHARE, Base64Codec.encode(share));
            }
        }
        return element;
    }

    public byte[] recover() throws IOException, GeneralSecurityException {
        final File fileExpression = new File(resourceExpression);
        final String expression = UTF8Codec.toString(StreamU.read(fileExpression));
        final Grammar grammar = new Grammar(expression);
        final Tree tree = new Tree(grammar.toNode());
        final MultiOperator operatorSecret = Optional.of(tree.getRoot())
                .map(MultiOperator.class::cast)
                .filter(m -> m.getOp().equals(Const.SECRET))
                .orElseThrow(IOException::new);
        final File fileShares = new File(resourceShares);
        final byte[] xml = StreamU.read(fileShares);
        final Document document = DocumentU.toDocument(xml);
        final Element elementSecret = document.getDocumentElement();
        final byte[] secret = recoverA(operatorSecret, elementSecret);
        verify(ElementU.getAttribute(elementSecret, Const.HASH), toHash(expression, secret), resourceExpression);
        return secret;
    }

    private byte[] recoverA(final MultiOperator operator, final Element element)
            throws IOException, GeneralSecurityException {
        final List<Node> operands = new ArrayList<>(operator.getOperands());
        final Operand operandThresholdN = (Operand) operands.remove(0);
        final int sharesN = operands.size();
        final int thresholdN = Integer.parseInt(operandThresholdN.getValue());
        final List<byte[]> shares = new ArrayList<>();
        final Iterator<Node> iterator = operands.iterator();
        for (Element elementChild : ElementU.getChildren(element)) {
            final byte[] share;
            final Node node = iterator.next();
            final MultiOperator operatorChild = (MultiOperator) node;
            final String op = operatorChild.getOp();
            if (Const.SECRET.equals(op)) {
                share = recoverA(operatorChild, elementChild);
            } else {
                final byte[] cipherText = Base64Codec.decode(ElementU.getTextContent(elementChild));
                final NameTypeValue nameTypeValue = registry.evaluate(operatorChild);
                share = unprotect(nameTypeValue, cipherText);
            }
            Optional.ofNullable(share).ifPresent(shares::add);
        }
        final DataSplitter dataSplitter = new DataSplitter(sharesN, thresholdN, null);
        return dataSplitter.join(shares.toArray(new byte[shares.size()][]));
    }

    private byte[] protect(
            final NameTypeValue ntv, final byte[] plainText) throws GeneralSecurityException {
        final String password = ntv.getValueS();
        final byte[] salt = HashU.sha256(UTF8Codec.toBytes(ntv.getName()));
        final SecretKey key = KeyU.toKeyPBE(Value.toCharArray(password), salt);
        final KeyCodec keyCodec = new KeyCodec(key, KeyX.Const.TRANSFORM_GCM, KeyX.Const.PARAM_SPEC_GCM, random);
        return keyCodec.encode(plainText);
    }

    private byte[] unprotect(
            final NameTypeValue ntv, final byte[] cipherText) throws GeneralSecurityException {
        final String password = ntv.getValueS();
        final byte[] salt = HashU.sha256(UTF8Codec.toBytes(ntv.getName()));
        final SecretKey key = KeyU.toKeyPBE(Value.toCharArray(password), salt);
        final KeyCodec keyCodec = new KeyCodec(key, KeyX.Const.TRANSFORM_GCM, KeyX.Const.PARAM_SPEC_GCM, random);
        try {
            return keyCodec.decode(cipherText);
        } catch (GeneralSecurityException e) {
            logger.info(String.format("FAILED TO RECOVER ATTRIBUTE: [%s]", ntv.getName()));
            return null;
        }
    }

    private String toHash(final String expression, final byte[] secret) {
        return Base64Codec.encode(HashU.sha256(ByteU.join(UTF8Codec.toBytes(expression), secret)));
    }

    private void verify(final String hashExpected, final String hashActual,
                        final String label) throws GeneralSecurityException {
        if (Value.equal(hashExpected, hashActual)) {
            logger.info(String.format("RECOVERED SECRET: [%s]", label));
        } else {
            logger.severe(String.format("FAILED TO RECOVER SECRET: [%s]", label));
            throw new KeyException(String.format("%s: [%s] != [%s]", label, hashExpected, hashActual));
        }
    }

    public static class Const {
        private static final String URI = "urn:arwo:envsec";
        private static final String SHARE = "share";
        private static final String SECRET = "secret";
        private static final String HASH = "hash";
    }
}
