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
  <div class="collapse navbar-collapse">
    <ul class="nav navbar-top-links navbar-right">
      
      <ifLoggedOut>
        <li>
          <a href="/login">
            login
          </a>
        </li>
      </ifLoggedOut>

      <ifLoggedIn>
      <li>
        <loggedInUser/>
      </li>
      <li>
        <a href="/sources/new">
          <i class="fa fa-plus fa-fw"></i>
        </a>
      </li>
      <li class="dropdown">
        <a class="dropdown-toggle" data-toggle="dropdown" href="#">
          <i class="fa fa-user fa-fw"></i>
          <i class="fa fa-chevron-down"></i>
        </a>
        <ul class="dropdown-menu dropdown-user">
          <li>
            <a data-method="delete" href="/logout" rel="nofollow">
              <i class="fa fa-sign-out fa-fw">
                Logout
              </i>
            </a>
          </li>
        </ul>
      </li>
      </ifLoggedIn>
      
    </ul>
  </div>
</nav>

