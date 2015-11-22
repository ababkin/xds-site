<html>
  <head>
    <title>xdataset</title>
    <link rel="stylesheet" type="text/css" href="/static/css/bootstrap.min.css"/>
    <link rel="stylesheet" href="//cdnjs.cloudflare.com/ajax/libs/metisMenu/2.2.0/metisMenu.min.css">
    <link rel="stylesheet" type="text/css" href="/static/css/sb-admin-2.css"/>
    <link rel="stylesheet" type="text/css" href="/static/css/font-awesome.min.css"/>
    <link rel="stylesheet" type="text/css" href="/sass/style.css"/>
    <link rel="stylesheet" type="text/css" href="/sass/overrides.css"/>


    <link rel="shortcut icon" type="image/png" href="/static/favicon.png"/>

    <script src="/static/js/jquery-2.1.4.min.js"></script>
    <script src="/static/js/bootstrap.min.js"></script>
    <script src="//cdnjs.cloudflare.com/ajax/libs/metisMenu/2.2.0/metisMenu.min.js"></script>
    <script src="/static/js/sb-admin-2.js"></script>
  </head>
  <body>
    <div id="wrapper">
      <apply template="_navbar"/>
      <flash type='error'/>
      <flash type='warning'/>
      <flash type='info'/>
      <flash type='success'/>

      <div class="container-fluid">
        <apply-content/>
      </div>
    </div>
  </body>
</html>
