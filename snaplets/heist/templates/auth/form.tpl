<dfForm action="${action}" role="form" class="login" method="${method}">

  <div class="form-group">
    <dfLabel ref="username" class="control-label">Username</dfLabel>
    <dfInputText ref="username" class="form-control" autofocus="autofocus"/>
    <span class="help-block">
      <dfErrorList ref="username"/>
    </span>
  </div>

  <div class="form-group">
    <dfLabel ref="password" class="control-label required">Password</dfLabel>
    <dfInputPassword ref="password" class="form-control"/>
    <span class="help-block">
      <dfErrorList ref="password"/>
    </span>

  </div>

  <div class="checkbox">
    <dfLabel class="control-label" ref="remember">
      <dfInputCheckbox ref="remember"/>
      Remember me
    </dfLabel>
  </div>

  <div class="form-group">
    <dfInputSubmit value="${submitText}" class="btn btn-primary" data-disable-with="Submitting..."/>
  </div>

</dfForm>

