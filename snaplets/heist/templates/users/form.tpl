<div class="col-md-6">
  <h2>Sign up</h2>
  <form role="form" class="new_user" id="new_user" action="/users" accept-charset="UTF-8" method="${method}">
    <div class="form-group">
      <label class="control-label" for="user_first_name">First name</label>
      <input autofocus="autofocus" class="form-control" type="text" name="first_name" id="user_first_name">
    </div>

    <div class="form-group">
      <label class="control-label" for="user_last_name">Last name</label>
      <input autofocus="autofocus" class="form-control" type="text" name="last_name" id="user_first_name">
    </div>

    <div class="form-group">
      <label class="control-label" for="user_username">Username</label>
      <input class="form-control" type="text" name="username" id="user_username">
    </div>

    <div class="form-group">
      <label class="control-label required" for="user_email">Email</label>
      <input class="form-control" type="email" value="" name="email" id="user_email">
    </div>
    <em>
      (8 characters minimum)
    </em>

    <div class="form-group">
      <label class="control-label required" for="user_password">Password</label>
      <input autocomplete="off" class="form-control" type="password" name="password" id="user_password">
    </div>
    
    <div class="form-group">
      <label class="control-label" for="user_password_confirmation">Password confirmation</label>
      <input autocomplete="off" class="form-control" type="password" name="password_confirmation" id="user_password_confirmation">
    </div>

    <div class="actions">
      <input type="submit" name="commit" value="${submitText}" class="btn btn-default">
    </div>
  </form>
  <a href="/login">Log in</a>
  <br>

</div>

