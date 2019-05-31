(function(){

function removeAccents ( data ) {
    return data.normalize('NFD').replace(/[\u0300-\u036f]/g, "");
}

var searchType = jQuery.fn.DataTable.ext.type.search;

searchType.string = function ( data ) {
    return ! data ?
        '' :
        typeof data === 'string' ?
            removeAccents( data ) :
            data;
};

searchType.html = function ( data ) {
    return ! data ?
        '' :
        typeof data === 'string' ?
            removeAccents( data.replace( /<.*?>/g, '' ) ) :
            data;
};

}());
