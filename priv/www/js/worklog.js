var worklog = {
    global:{
        pageSize:5,
        selected:[],
        checkedTag:'<span class="glyphicon glyphicon-ok pull-right" aria-hidden="true"></span>'
    },
    autoResizeOption:{rows:1,cols:75, maxRow:10},
    getReportObj:function(objId){
        return $("<tr id="+ objId +"><th></th><td><textarea class=\"form-control\" rows=\"1\" placeholder=\"Enter your today's report\" name=\"content\" data-container=\"body\" data-toggle=\"popover\" data-placement=\"right\"></textarea></td><td><input  maxlength=\"4\" name=\"timeSpent\" type=\"text\" class=\"form-control\" placeholder=\"hours\" data-container=\"body\" data-toggle=\"popover\" data-placement=\"right\"></td><td class=\"txt-center\"> <button type=\"button\" name=\"close\" title=\"remove this report\" class=\"btn btn-default btn-xs\">X</button></td></tr>");
    }
};
           
//$.validator.setDefaults({  ignore: ":hidden" });
$.validator.addMethod("timeSpent", function(value, element) {       
     return this.optional(element) || /^\d+(.\d)?$/.test(value);       
 }, "After the decimal point, only one digit can be kept."); 

$(document).ready(function(){

    $('#loginForm').validate({
        rules: {
            username: {
                minlength: 4,
                maxlength: 20, 
                required: true
            },
            password: {
                minlength: 6,
                maxlength: 16, 
                required: true
            }
        },

        errorElement: 'span',
        errorClass: 'help-block',
        highlight: function(element) {
            $(element).addClass('has-error');
        },
        unhighlight: function(element) {
            $(element).removeClass('has-error');
        },
        submitHandler:function(form){
            var loginForm$ = $(form);
            var $btn = loginForm$.find("#loginBtn").button('loading');
             $.ajax({
                url: loginForm$.attr("action"),
                method:"POST", 
                dataType: "json",
                cache:false,
                data:loginForm$.serialize(),
                success: function(data, status, xhr){
                    closeLogin();
                    showLoading("Init user, please waiting...");
                    setUser(data);
                    orderInit();
                },
                error: function(xhr,responseText, msg){
                    $btn.button('reset');
                    var loginTip = $("#loginTip");
                    loginTip.html("Login failed! message:" +responseText + ";" + msg);
                    loginTip.fadeIn("slow", function(){
                        window.setTimeout(function(){
                            loginTip.fadeOut("slow");
                        }, 3000);
                    });
                }
               
            });

            return false;
        }

    });

    initUser().done(orderInit).fail(closeLoading).fail(showLogin);
     
    $("#selProject, #selStatus").change(function(){
        if(!$("#collapse").data.init){
            $("#collapse").data("init", true);
        }
        initPagination();
    });

    $("#issues-wrap").on("click", "#issues-tb :button", function(){
        var btn = $(this), btnTd = btn.parent(),summaryTd = btnTd.prev(), 
        curIssue = summaryTd.prev().find("a"),
        flyWrap = $('<p class="issue-summary"></p>'),
        summaryI = $('<i></i>');
        summaryI.html(btnTd.prev().text());
        flyWrap.append(curIssue.clone()).append(summaryI);

        flyWrap.css({
            'z-index': 9000,
            'display': 'block',
            'position': 'absolute',
            'top': curIssue.offset().top +'px',
            'left': curIssue.offset().left +'px',
            'width': curIssue.width() + summaryTd.width() +'px',
            'height': curIssue.height() + summaryTd.height() +'px'
        });
        var reportObj = $("#"+ curIssue.text());
        var exist = reportObj.length !== 0;
        if (!exist) {
            var objId = curIssue.text();
            worklog.global.selected.push(objId);
            reportObj = worklog.getReportObj(objId);
            reportObj.appendTo("#report-tb").data({key:objId, action:"create"});
            reportObj.find("textarea").textareaAutoResize(worklog.autoResizeOption);
        }else{
            if (reportObj.data().action === "delete") {
                reportObj.append(reportObj.data().children);
                reportObj.data().action = "update";
                reportObj.show();
            } 
        }
        var reportIssue = reportObj.find("th");
        $('body').append(flyWrap);
        flyWrap.animate({
            top: reportIssue.offset().top,
            left: reportIssue.offset().left,
            width: 20,
            height: 10,
            display: "none"
        }, 'slow', function() {
            exist?flyWrap.remove():(flyWrap.appendTo(reportIssue),flyWrap.removeAttr('style'), summaryI.css("display", "block").html(formatSummary(summaryI.html())));

        });
        if (btn.next().length === 0) {
            btn.after(worklog.global.checkedTag);
         }
       
    });

    $("#collapse").on("click", function(){
        var collapse$ = $(this);
        var issuesSelect = function(){
            var wrap$ = $("#issues-wrap")
                if(collapse$.hasClass("glyphicon-collapse-up")){
                    wrap$.slideDown("slow", function(){
                        collapse$.removeClass("glyphicon-collapse-up").addClass("glyphicon-collapse-down");
                        wrap$.next().fadeIn();
                    });
                }else{
                    wrap$.slideUp("slow", function(){
                        collapse$.removeClass("glyphicon-collapse-down").addClass("glyphicon-collapse-up");
                        wrap$.next().slideUp("fast");
                    });
                }
        };
        if(!collapse$.data().init) {
            initProjects().done(function(){initPagination();collapse$.data("init", true); issuesSelect();}).fail(closeLoading);
        }else{
            issuesSelect();
        }
    });

    $("#loginOut").on("click", function(){
        sendDelete("api/v1/jira/login", {"content-type":"application/json"}, function(){
            window.location.href = "/login.html";
        });
    });

    $("#reports").on("click", "button[name=close]", function(e){
        var rself$ = $(this);
        var wrap$ = rself$.closest("tr");
        wrap$.fadeOut("slow", function(){
            if (wrap$.data("action") === "create") {
                wrap$.remove();
            }else{
                wrap$.data("action", "delete");
                wrap$.data("children", wrap$.children().detach());
            }
            key = wrap$.attr("id");
            worklog.global.selected.remove(key);
            var temp = $("#issues-tb :button[name="+key+"]");
            temp.length === 0 || temp.next().remove();
            
        });
    });

    $("#add-other input[type=text]").on("keydown", function(e){
        e.which === 13 && $(this).next().trigger("click");
    });

    $("#add-other span.btn").on("click", function(){
        var $inputTxt = $(this).prev();
        var keyword = $inputTxt.val();
        if ($.trim(keyword) === "") {
            $inputTxt.addClass("has-error");
            window.setTimeout(function(){$inputTxt.removeClass("has-error");}, 1000);
            return;
        }

        var query = {jql:"key='"+ keyword +"'" ,fields:["id", "key", "summary", "issuetype"]};
        initPagination(query);
        //searchIssues(query, showIssues).done(closeLoading).fail(function(){ alert("not found issue");closeLoading();});
    });

    $("#reportForm").validate({
        rules: {
            content: {
                minlength: 4,
                required: true
            },
            timeSpent: {
                number:true,
                // digits:true,
                timeSpent:true,
                maxlength:4,
                range:[0.5,24],
                required: true
            }
        },

        highlight: function(element) {
            $(element).addClass('has-error');
        },
        unhighlight: function(element) {
            $(element).removeClass('has-error').popover("destroy");
        },
        submitHandler:submitReport,
        errorPlacement:function(error,element){
            $(element).popover("destroy").popover({"content":error.text(), "trigger": "hover"});
        }

    });

    $(document).click(function(){
        $("#collapse").popover("destroy");
    });
});



function showLoading(tip, noloadImg){
    var loadingModal = $("#loadingModal");
    noloadImg ? loadingModal.find("img").hide():
    loadingModal.find("img").show();
    
    tip && loadingModal.find("span").html(tip);
    loadingModal.modal({'backdrop':'static', 'keyboard':false});
};

function closeLoading(){
    $("#loadingModal").modal("hide");
};

function showLogin(){
    $("#myModal").modal({'backdrop':'static', 'keyboard':false});
};
function closeLogin(){
    $("#myModal").modal("hide");
};

function orderInit(){
    var  init = function(){
        $("#reportForm textarea").textareaAutoResize({rows:1,cols:75, maxRow:10});
        closeLoading();
        $("#collapse").popover({"content":"Hey, Welcome DDRT. Click here to add more issue !"}).popover('show');
        //initProjects().done(function(){initPagination();}).fail(closeLoading);
     }; 
    loadTodayIssues(init);
};


function getBrowseUrl(key){
    return "http://jira/browse/" + key;
};

function isSelected(key){
    return $.inArray(key, worklog.global.selected) !== -1;
};


var pageCallback = function(index, jq) { 
    initIssue(index).then(closeLoading);
}; 



var showIssues = function(data, keepOld){
    var issueWrap = $("#issues-tb");
    !keepOld && issueWrap.children().remove();
    $.each(data.issues, function(k, v){
        issueWrap.append(('<tr><th scope="row">'+ (k + 1)
                        +'</th><td><img src="'
                        + v.fields.issuetype.iconUrl+'" height="16" width="16" border="0" align="absmiddle" title="'
                        + v.fields.issuetype.description+'">'
                        + v.fields.issuetype.name+'</td><td><a href="'+ getBrowseUrl(v.key) +'" target="_blank">'
                        + v.key +'</a></td><td>'
                        + (v.fields.summary || "N/A") +'</td><td><button type="button" name='
                        + v.key +' class="btn btn-default btn-xs font-bold" title="add a report"> + </button>'
                        + (isSelected(v.key)?worklog.global.checkedTag:'') + '</td></tr>'));
     });
 };
//<span class="glyphicon glyphicon-ok pull-right" aria-hidden="true"></span>
var initPagination = function(query){
    var query = query || buildJQL();
    var issueWrap = $("#issues-tb");
    issueWrap.children().remove();

    return  sendAjax("api/v1/jira/search", 
                function(){showLoading("Loding issue, please waiting...");}, function(data){
                showIssues(data);
               $("#pagination").pagination(data.total, {
                    callback: pageCallback, 
                    prev_text: "prev",
                    next_text: "next",
                    items_per_page: worklog.global.pageSize,
                    num_edge_entries: 2,
                    num_display_entries: 5,
                    show_if_single_page: true,
                    load_first_page:false,
                    link_to: "javascript:void(0)"
                });
            }, false, "POST", JSON.stringify(query)).done(closeLoading).fail(function(){ 
                showLoading("Not found issue".fontcolor("red"), true);
                window.setTimeout(closeLoading, 1000);});
};


var initIssue = function(pageIndex){
    var query = buildJQL();
    query.startAt = pageIndex * worklog.global.pageSize;
    return searchIssues(query, showIssues);
    
};

var searchIssues = function(query, setResult){
    return sendAjax("api/v1/jira/search", function() {showLoading("Loding issue, please waiting...")}, function(data){
       setResult(data);
    }, false, "POST", JSON.stringify(query));
};

var submitReport = function(form){
    //var reports = [], item;
    var trWraps = $("#reports table tbody tr:visible");
    if (trWraps.size() === 0) {
        alert("At least one report, please add from Issues!");
        return;
    }
    showLoading("submit, please waiting...");
    var reportObj = {"create": [], "update":[], "delete":[]},tr$, action="";
    $("#reports table tbody tr").each(function(){
        var tr$ = $(this);
        action = tr$.data("action");
        item = buildItem(tr$, action);
        reportObj[action].push(item);
    });
     var userInfo = $("#curUser");
     reportObj.userId = userInfo.data().userId;
     reportObj.username = userInfo.data().displayName;
     reportObj.loginId = userInfo.data().name;
    //var reportObj = {"reports": reports, "userId":$("#curUser").data().userId};
    $.post("api/v1/jira/worklog",  JSON.stringify(reportObj)).done(function(data){
        showLoading("submit success", true);
        resetForm(data, form);
        window.setTimeout(closeLoading, 3000);
    }).fail(function(){
        showLoading("Worklog failed, Please try again later!".fontcolor("red"), true);
        window.setTimeout(closeLoading, 3000);
     });
   return false;
};

function resetForm(data, form){
    //form.reset();
     var responseObj = eval(data);
    $("#reports table tbody tr").each(function(){
        var $tr = $(this);
        if ($tr.is(":hidden")) {
            $tr.remove();
        } else{
            if($tr.data().action === "create"){
                $tr.data("worklogId",  findLodId(responseObj, $tr.data().key));
            }
            $tr.data().action = "update";
        }
    });
};

function findLodId(responseObj, issue){
    for (var i = 0; i < responseObj.length; i++) {
        if (responseObj[i].issue === issue) {
            return responseObj[i].id;
        }
    }
    return 0;
};

var buildItem = function(item$, action){
    //var item$ = $(item);
    var logId = item$.data("worklogId");
    var reportItem = {};
    reportItem.key = item$.data("key");
    if (action === "delete") {
        if (!logId) {
            alert("data error");
        }
        reportItem.id = logId;
        return reportItem;
    }
    reportItem.comment = item$.find("textarea[name=content]").val();
    reportItem.timeSpent = item$.find("input:text[name=timeSpent]").val();
    if(logId){ reportItem.id = logId ;}
    return reportItem;
}


var buildJQL = function(){
    var jql = "assignee='" + $("#curUser").data().name + "' and ";
    var project$ = $("#selProject option:selected");
    if (project$.val() != -1) {
        jql += "project='" + project$.text() + "' and ";
    }
    //var status = $("#issues-wrap input:checkbox[name=status]:checked");
    // if (status.size() < 1) {
    //     //alert("At least choose a status");
    //     return;
    // }
    // var statusArr = [];
    // status.each(function(){
    //     statusArr.push(this.value);
    // });
    //jql += "status in ('" + statusArr.join("','") + "') order by created, priority desc";
    jql += "status='" + $("#selStatus option:selected").text() + "'";
    var query = {};
    query.jql = jql;
    query.startAt = 0;
    query.maxResults = worklog.global.pageSize;
    query.fields = ["id", "key", "summary", "issuetype"];
    return query;
};

var sendDelete = function(url, headers, success_call){
    $.ajax({
          type: "DELETE",
          url: url,
          cache:false,
          dataType:"json",
          headers:headers,
          success: success_call
      }); 
 };

var initUser = function(){
    return sendAjax("/api/v1/jira/user",
    function(){
        showLoading("Check login, please waiting...");
    }, setUser, false);
};

 var initProjects =  function(){
    return sendAjax("api/v1/jira/project",
     function(){
        showLoading("Init projects, please waiting...");
    },
    function(data){
        var projectSel = $("#selProject");
        projectSel.empty();
        projectSel.append('<option value="-1">All</option>');
        $.each(data, function(k,v){
            projectSel.append('<option key='+ v.key +' value="' +v.id+ '">'+v.name+'</option>');
        });
    });
};

var loadTodayIssues = function(init){
    //var dtd = $.Deferred();
    sendAjax("api/v1/report/user/"+$("#curUser").data().userId+"/1",
     function(){
        showLoading("Init interface, please waiting...");
    },
    function(data){
       //console.log(data);
        if (data.length === 0) {
            return searchOldLogedIssue(init);
        }

        var keys = [];
        $.each(data, function(){
            keys.push(this.issue);
        });

        var jql = "key in ('" + keys.join("','") + "') order by created, priority desc";
        var query = {jql:jql ,fields:["id", "key", "summary"]};
        searchIssues(query, function(jira_data){
            initInterface(data, jira_data);
        }).then(function(){
            init();
        });
    }, false).fail(function(){
        init();
    } );

   // return dtd.promise();
};

var initInterface= function(data, jira_data) {
    var len = data.length, reportObj = false, objId;
    for (var i = 0; i < len; i++) {
        objId = data[i].issue;
        reportObj = worklog.getReportObj(objId);
        reportObj.find("th").html('<p class="issue-summary"><a href="'+getBrowseUrl(objId)+'" target="_blank">'
            +objId+'</a><i style="display: block;">'+ formatSummary(findIssue(jira_data.issues, objId).fields.summary || "") +'</i></p>');
        reportObj.find("textarea").val(data[i].content || "").textareaAutoResize(worklog.autoResizeOption);
        reportObj.find("input[name=timeSpent]").val(data[i].timeSpent || "");
        reportObj.appendTo("#report-tb").data({key:objId, worklogId:data[i].worklogId + "", action:"update"});
        worklog.global.selected.push(objId);
    }
    
};

var initPrevInterface= function(data, jira_data) {
    var len = data.length, reportObj = false, objId;
    for (var i = 0; i < len; i++) {
        objId = data[i].issue;
        reportObj = worklog.getReportObj(objId);
        reportObj.find("th").html('<p class="issue-summary"><a href="'+getBrowseUrl(objId)+'" target="_blank">'
            +objId+'</a><i style="display: block;">'+ formatSummary(findIssue(jira_data.issues, objId).fields.summary || "") +'</i></p>');
        reportObj.appendTo("#report-tb").data({key:objId, action:"create"});
        reportObj.find("textarea").textareaAutoResize(worklog.autoResizeOption);
        worklog.global.selected.push(objId);
    }
    
};

var initInterfaceHistory= function(jira_data) {
    var len = jira_data.issues.length, reportObj = false;
    $.each(jira_data.issues, function(){
        reportObj = worklog.getReportObj(this.key);
        reportObj.find("th").html('<p class="issue-summary"><a href="'+getBrowseUrl(this.key)+'" target="_blank">'
            +this.key+'</a><i style="display: block;">'+ (this.fields.summary || "") +'</i></p>');
        reportObj.appendTo("#report-tb").data({key:this.key, action:"create"});
        reportObj.find("textarea").textareaAutoResize(worklog.autoResizeOption);
        worklog.global.selected.push(this.key);
    });
    
};

var findIssue = function(issues, key){
    for (var i = 0; i < issues.length; i++) {
        if (issues[i].key === key) {
            return issues[i];
        }
    }
    return false;
};

var formatSummary = function(summary){
    var realLength = 0, len = summary.length, charCode = -1;
    for(var i = 0; i < len; i++){
        charCode = summary.charCodeAt(i);
        if (charCode >= 0 && charCode <= 128) { 
            realLength += 1;
        }else{ 
            //if is chinese
            realLength += 2;
        }
        if (realLength > 75) {
            return summary.substring(0, i) + "...";
        }
    } 
    return summary;
};

//"assignee=hh49 and issueFunction in workLogged('before 2015/08/5 by hh49')"
//issueFunction in workLogged('after -1w by hh49')

function searchOldLogedIssue(init){
    sendAjax("api/v1/issue/prev/"+$("#curUser").data().userId,
     function(){
        showLoading("Init interface, please waiting...");
    },
    function(data){
        if (data.length === 0) {
            var curUser = $("#curUser").data().name;
            var jql = "assignee = "+ curUser +" and issueFunction in workLogged('after -1d by " + curUser +"') and status != Closed";
            var query = {jql:jql ,fields:["id", "key", "summary"]};
            return searchIssues(query, initInterfaceHistory).done(init).fail(init);
        }

        var keys = [];
        $.each(data, function(){
            keys.push(this.issue);
        });

        var jql = "key in ('" + keys.join("','") + "') order by created, priority desc";
        var query = {jql:jql ,fields:["id", "key", "summary"]};
        searchIssues(query, function(jira_data){
            initPrevInterface(data, jira_data);
        }).then(function(){
            init();
        });
    }, false)

    
};

var setUser = function(data){
    var titlePanle$ = $("#reportUser");
    titlePanle$.find("img").attr("src", data.avatarUrls["16x16"]).next().html(data.displayName).data(data);
};

function sendAjax(url, beforeSend, successCallback, errorCallback, method, data, dataType){
    var dtd = $.Deferred();
    $.ajax({
            url: url,
            method: method || "GET", 
            data: data,
            dataType: dataType || "json",
            beforeSend:beforeSend,
            success: function(data, textStatus, jqXHR){
                if (successCallback && typeof successCallback === "function") {
                    successCallback(data, textStatus, jqXHR);
                }
                dtd.resolve();
            },
            error: function(jqXHR, textStatus, errorThrown){
                if (errorCallback && typeof errorCallback === "function") {
                    errorCallback(jqXHR, textStatus, errorThrown);
                }
                dtd.reject();
            }  
        });
    return dtd.promise();
};