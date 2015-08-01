var setCookie = function(c_name,value,expiredays){
            var exdate=new Date()
            exdate.setDate(exdate.getDate()+expiredays)
            cookieVal=c_name+ "=" +escape(value)+((expiredays==null) ? "" : ";expires="+exdate.toGMTString());
            //alert(cookieVal);
            document.cookie=cookieVal;
        };

//get cookie
var getCookie = function(c_name){
    if (document.cookie.length>0){
        c_start=document.cookie.indexOf(c_name + "=")
    if (c_start!=-1){ 
        c_start=c_start + c_name.length+1 
        c_end=document.cookie.indexOf(";",c_start)
        if (c_end==-1) c_end=document.cookie.length
        //document.write(document.cookie.substring(c_start,c_end)+"<br>");
        return unescape(document.cookie.substring(c_start,c_end))
        } 
    }
    return ""
};

Array.prototype.remove=function(element){
    var index = $.inArray(element, this);
    if (index === -1) {
        return;
    }
    this.splice(index, 1);
};

function getStrLen(str){
    var realLength = 0;
    var len = str.length;
    var charCode = -1;
    for(var i = 0; i < len; i++){
        charCode = str.charCodeAt(i);
        if (charCode >= 0 && charCode <= 128) { 
            realLength += 1;
        }else{ 
            //if is chinese
            realLength += 2;
        }
    } 
    return realLength;
};
