<dfForm action="${action}" role="form" class="user" method="${method}">

  <div class="form-group">
    <dfLabel ref="first_name" class="control-label">First Name</dfLabel>
    <dfInputText ref="first_name" class="form-control" autofocus="autofocus"/>
  </div>

  <div class="form-group">
    <dfLabel ref="last_name" class="control-label">Last Name</dfLabel>
    <dfInputText ref="last_name" class="form-control"/>
  </div>

  <div class="form-group">
    <dfLabel ref="username" class="control-label required">Username</dfLabel>
    <dfInputText ref="username" class="form-control"/>
    <span class="help-block">
      <dfErrorList ref="username"/>
    </span>
  </div>

  <div class="form-group">
    <dfLabel ref="email" class="control-label required">Email</dfLabel>
    <dfInputText ref="email" class="form-control"/>
    <span class="help-block">
      <dfErrorList ref="email"/>
    </span>
  </div>

  <div class="form-group">
    <dfLabel ref="password.password" class="control-label required">Password</dfLabel>
    <em> (8 characters minimum) </em>
    <dfInputPassword ref="password.password" class="form-control"/>
    <span class="help-block">
      <dfErrorList ref="password.password"/>
    </span>
  </div>

  <div class="form-group">
    <dfLabel ref="password.password_confirmation" class="control-label required">Password Confirmation</dfLabel>
    <dfInputPassword ref="password.password_confirmation" class="form-control"/>
    <span class="help-block">
      <dfErrorList ref="password"/>
    </span>
    <span class="help-block">
      <dfErrorList ref="password.password_confirmation"/>
    </span>
  </div>


  <div class="form-group">
    <dfInputSubmit value="${submitText}" class="btn btn-primary" data-disable-with="Submitting..."/>
  </div>

</dfForm>


