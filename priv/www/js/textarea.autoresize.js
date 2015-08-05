;(function($){
    $.fn.textareaAutoResize = function(options){
        var settings = $.extend({
            rows: 1,
            cols: 20,
            maxRow:10
        }, options);

        return this.each(function(){
          var $textWrap = $(this);
          $textWrap.data(settings);
          $textWrap.attr("rows",options.rows).attr("cols", options.cols);
          $textWrap.on("keydown", resizeIt);
          $textWrap.trigger("keydown");
        });
    };
    var resizeIt = function(){
      var $textarea = $(this);
      var str =  $textarea.val();
      var cols =   $textarea.prop("cols");

      var linecount = 0;
      var rows = str.split("\n");

      $.each(rows, function() {
          if (this.length === 0) {
            linecount += 1;
           }else{
              linecount += Math.ceil( this.length / cols );
           }
      });
      $textarea.data().maxRow > linecount &&
      $textarea.attr("rows", linecount);
    };
  })(jQuery);