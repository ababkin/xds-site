<div class="col-md-6">
  <div class="alert alert-danger" role="alert"><loginError/></div>
  <h2>Log in</h2>
  <form role="form" class="new_user" id="new_user" action="/login" accept-charset="UTF-8" method="post">
    <div class="form-group">
      <label class="control-label required" for="user_login">Email</label>
      <input autofocus="autofocus" class="form-control" type="login" value="" name="login" id="user_login" autocomplete="off">
    </div>
    <div class="form-group">
      <label class="control-label required" for="user_password">Password</label>
      <input autocomplete="off" class="form-control" type="password" name="password" id="user_password">
    </div>
    <div class="checkbox">
      <label for="user_remember_me">
        <input name="remember_me" type="hidden" value="0">
        <input type="checkbox" value="1" name="remember_me" id="user_remember_me"> 
        Remember me
      </label>
    </div>

    <div class="actions">
      <input type="submit" name="commit" value="Log in" class="btn btn-default">
    </div>
  </form>
  <a href="/signup">Sign up</a>
  <br>
  <a href="/users">Forgot your password?</a>
  <br>
</div>
