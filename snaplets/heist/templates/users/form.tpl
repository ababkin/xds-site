<dfForm action="/users" role="form" class="user" method="${method}">
  <dfChildErrorList/>

  <div class="form-group">
    <dfLabel ref="first_name" class="control-label">First Name</dfLabel>
    <dfInputText ref="first_name" class="form-control" autofocus="autofocus"/>
  </div>

  <div class="form-group">
    <dfLabel ref="last_name" class="control-label">Last Name</dfLabel>
    <dfInputText ref="last_name" class="form-control"/>
  </div>

  <div class="form-group">
    <dfLabel ref="login" class="control-label required">Login</dfLabel>
    <dfInputText ref="login" class="form-control"/>
  </div>

  <div class="form-group">
    <dfLabel ref="email" class="control-label required">Email</dfLabel>
    <dfInputText ref="email" class="form-control"/>
  </div>

  <div class="form-group">
    <dfLabel ref="password" class="control-label required">Password</dfLabel>
    <em> (8 characters minimum) </em>
    <dfInputText ref="password" class="form-control"/>
  </div>

  <div class="form-group">
    <dfLabel ref="password_confirmation" class="control-label required">Password Confirmation</dfLabel>
    <dfInputText ref="password_confirmation" class="form-control"/>
  </div>


  <div class="form-group">
    <dfInputSubmit value="${submitText}" class="btn btn-primary" data-disable-with="Submitting..."/>
  </div>

</dfForm>


