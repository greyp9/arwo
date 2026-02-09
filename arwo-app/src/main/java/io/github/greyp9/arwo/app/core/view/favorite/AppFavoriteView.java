package io.github.greyp9.arwo.app.core.view.favorite;

@SuppressWarnings("PMD.ExcessiveImports")
public class AppFavoriteView {
    // private final ServletHttpRequest httpRequest;
    // private final AppUserState userState;
    // private final XedCursor cursorType;
    // private final Bundle bundle;
    // private final String type;

/*
    public AppFavoriteView(final ServletHttpRequest ignoredHttpRequest, final AppUserState ignoredUserState,
                           final XedCursor ignoredCursorType, final String ignoredType) {
        // this.httpRequest = httpRequest;
        // this.userState = userState;
        // this.cursorType = cursorType;
        // this.bundle = ((cursorType == null) ? null : cursorType.getXed().getBundle());
        // this.type = type;
    }

    public final HttpResponse addContentTo(final Element html) throws IOException {
        final String menuKey = Value.join(Http.Token.SLASH,
                httpRequest.getServletPath(), type, AppMenuFactory.Const.FAVORITES);
        if ((cursorType != null) && (userState.getMenuSystem().isOpen(menuKey))) {
            addContentInner(html);
        }
        return null;
    }

    private void addContentInner(final Element html) throws IOException {
        final RowSetMetaData metaData = createMetaData();
        final RowSet rowSet = createRowSet(metaData);
        final Locus locus = userState.getLocus();
        final ViewState viewState = userState.getViewStates().getViewState(metaData, bundle, locus);
        final Table table = new Table(rowSet, viewState.getSorts(), viewState.getFilters(), null, null);
        TableU.addFooterStandard(table, bundle);
        final XedActionFilter filter = new XedActionFilter(userState.getXedFactory(), userState.getLocale());
        final TableContext tableContext = new TableContext(
                viewState, filter, userState.getSubmitID(), App.CSS.TABLE, bundle, userState.getLocus());
        final TableView tableView = new TableView(table, tableContext);
        tableView.addContentTo(html);
    }

    private RowSetMetaData createMetaData() {
        final RowSetMetaData metaDataEnabled = new XedMetaDataFactory().create(cursorType.getTypeInstance(), false);
        final RowSetMetaData metaData = RowSetMetaDataU.removeColumn(metaDataEnabled, App.Settings.ENABLED);
        final ColumnMetaData columnAction = new ColumnMetaData(App.Action.SELECT, Types.VARCHAR, false);
        return RowSetMetaDataU.addLeft(metaData, columnAction);
    }

    private RowSet createRowSet(final RowSetMetaData metaData) throws IOException {
        final RowSet rowSet = new RowSet(metaData, null, null);
        final TypeInstance typeInstance = cursorType.getTypeInstance();
        final XedCursor parentConcrete = cursorType.getParentConcrete();
        final XedNav nav = new XedNav(cursorType.getXed());
        final Collection<Element> children = parentConcrete.getChildren(typeInstance);
        for (final Element child : children) {
            final XedCursor childCursor = nav.find(child, parentConcrete);
            final TypeInstance instanceEnabled = childCursor.getTypeInstance().getInstance(App.Settings.ENABLED);
            if (Boolean.parseBoolean(childCursor.getValue(instanceEnabled))) {
                createRow(rowSet, metaData, childCursor);
            }
        }
        return rowSet;
    }

    @SuppressWarnings("PMD.AvoidInstantiatingObjectsInLoops")
    private void createRow(final RowSet rowSet, final RowSetMetaData metaData, final XedCursor item) {
        final InsertRow insertRow = new InsertRow(rowSet);
        for (final Iterator<ColumnMetaData> it = metaData.iterator(); it.hasNext(); it.getClass()) {
            final ColumnMetaData columnMetaData = it.next();
            if (columnMetaData.getName().equals(App.Action.SELECT)) {
                final SubmitToken token = new SubmitToken(App.Target.SESSION, App.Action.SELECT_FAV, item.getURI());
                insertRow.setNextColumn(new TableViewButton(UTF16.SELECT, userState.getSubmitID(), token.toString()));
            //} else if (!columnMetaData.getName().equals(App.Settings.ENABLED)) {
            } else {
                final TypeInstance typeInstance = item.getTypeInstance().getInstance(columnMetaData.getName());
                insertRow.setNextColumn(item.getValue(typeInstance));
            }
        }
        rowSet.add(insertRow.getRow());
    }
*/
}
