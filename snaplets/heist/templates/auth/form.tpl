<dfForm action="${action}" role="form" class="login" method="${method}">

  <dfIfErrors ref="credentials">
    <div class="alert alert-danger" role="alert">
      <dfErrorList ref="credentials"/>
    </div> 
  </dfIfErrors>

  <div class="form-group">
    <dfLabel ref="credentials.username" class="control-label">Username</dfLabel>
    <dfInputText ref="credentials.username" class="form-control" autofocus="autofocus"/>
    <span class="help-block">
      <dfErrorList ref="credentials.username"/>
    </span>
  </div>

  <div class="form-group">
    <dfLabel ref="credentials.password" class="control-label required">Password</dfLabel>
    <dfInputPassword ref="credentials.password" class="form-control"/>
    <span class="help-block">
      <dfErrorList ref="credentials.password"/>
    </span>

  </div>

  <div class="checkbox">
    <dfLabel class="control-label" ref="credentials.remember">
      <dfInputCheckbox ref="credentials.remember"/>
      Remember me
    </dfLabel>
  </div>

  <div class="form-group">
    <dfInputSubmit value="${submitText}" class="btn btn-primary" data-disable-with="Submitting..."/>
  </div>

</dfForm>

