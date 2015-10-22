<nav class="navbar navbar-default navbar-static-top" role="navigation" style="margin-bottom: 0">
  <div class="navbar-header">
    <button class="navbar-toggle" data-target=".navbar-collapse" data-toggle="collapse" type="button">
      <span class="sr-only">Toggle navigation</span>
      <span class="icon-bar"></span>
      <span class="icon-bar"></span>
      <span class="icon-bar"></span>
    </button>
    <a class="navbar-brand" href="/">
      <span class="join-symbol">⨝</span>
      <span class="dataset">dataset</span>
    </a>
  </div>

  <ul class="nav navbar-top-links navbar-right">
  <ifLoggedOut>
    <a class="btn btn-default" rel="nofollow" href="/login">
      Login
    </a>
  </ifLoggedOut>

  <ifLoggedIn>
    <li class="dropdown">
      <a class="dropdown-toggle" data-toggle="dropdown" href="#">
        <i class="fa fa-user fa-fw"></i>
        <i class="fa fa-caret-down"></i>
      </a>
      <ul class="dropdown-menu dropdown-user">
        <li>
          <a rel="nofollow" data-method="delete" href="/logout">
            <i class="fa fa-sign-out fa-fw"></i>
            Logout
          </a>
        </li>
      </ul>
    </li>
  </ifLoggedIn>
  </ul>
</nav>

