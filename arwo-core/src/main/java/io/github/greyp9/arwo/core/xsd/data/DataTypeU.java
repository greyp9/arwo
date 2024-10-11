package io.github.greyp9.arwo.core.xsd.data;

public final class DataTypeU {

    private DataTypeU() {
    }

    public static DataType getRootBaseType(final DataType dataType) {
        DataType dataTypeIt = dataType;
        while ((dataTypeIt != null) && (dataTypeIt.getBaseType() != null)) {
            dataTypeIt = dataTypeIt.getBaseType();
        }
        return dataTypeIt;
    }
}
