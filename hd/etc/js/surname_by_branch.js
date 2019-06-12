$(document).ready(function() {
  $(document).on( "click", ".toggle_all", function() {
    $( "#surname_by_branch .toggle" ).each( function() {
      if( !$(this).hasClass("toggled") ) {
        $(this).html( "&#x2295;" ).addClass("toggled");
        $(this).closest( "li" ).children( "ul" ).hide().removeClass("hidden");
      }
    });
  });

  $(document).on( "click", ".untoggle_all", function() {
    $( "#surname_by_branch .toggled" ).each( function() {
      $(this).html( "&#x229D;" ).removeClass("toggled");
      $(this).closest( "li" ).children( "ul" ).show();
    });
  });

  $(document).on( "click", ".toggle_fix", function() {
    var param_u = "";
    $( "#surname_by_branch .toggled" ).each( function() {
      param_u += "&u=" + $(this).attr( "title" );
    });
    $(this).attr("href", this.href + param_u);
  });

  $(document).on( "click", "#surname_by_branch .toggle", function() {
    if( $(this).hasClass("toggled") ) {
      $(this).html( "&#x229D;" ).removeClass("toggled");
      $(this).closest( "li" ).children( "ul" ).show();
    } else {
      $(this).html( "&#x2295;" ).addClass("toggled");
      $(this).closest( "li" ).children( "ul" ).hide().removeClass("hidden");
    }
  });
});
