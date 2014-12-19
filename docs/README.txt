
GET http://localhost:port/rest/api/v1/reports/{groupid}/{date}
<email-content>


PUT http://localhost:port/rest/api/v1/report
{
‘userid':'by46',
'date':'2014-10-10',
'content': ''
}

GET http://localhost:port/rest/api/v1/users
[{
‘userid':'by46',
‘email':'benjamin.c.yan@newegg.com'
}]

GET http://localhost:port/rest/api/v1/user/{userid}
{
‘userid':'by46',
‘email':'benjamin.c.yan@newegg.com'
}

GET http://localhost:port/rest/api/v1/users/alertion/{groupid}/{date}
{
‘userid':'by46',
‘email':'benjamin.c.yan@newegg.com'
}

