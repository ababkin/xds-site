<dfForm action="${action}" role="form" class="login" method="${method}">
  <dfChildErrorList/>

  <div class="form-group">
    <dfLabel ref="username" class="control-label">Username</dfLabel>
    <dfInputText ref="username" class="form-control" autofocus="autofocus"/>
  </div>

  <div class="form-group">
    <dfLabel ref="password" class="control-label required">Password</dfLabel>
    <dfInputPassword ref="password" class="form-control"/>
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

