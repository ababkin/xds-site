<dfForm action="${action}" role="form" class="source" method="${method}">

  <div class="form-group">
    <dfLabel ref="title" class="control-label required">Title*</dfLabel>
    <dfInputText class="form-control" ref="title" placeholder="Enter source title"/>
    <span class="help-block">
      <dfErrorList ref="title"/>
    </span>
  </div>

  <div class="form-group">
    <dfLabel ref="description" class="control-label">Description*</dfLabel>
    <dfInputTextArea class="form-control" ref="description" cols="20" rows="3" placeholder="Enter brief description"/>
    <span class="help-block">
      <dfErrorList ref="description"/>
    </span>
  </div>



  <div class="form-group" >
    <dfLabel ref="url.webpage" class="control-label required">Webpage URL</dfLabel>
    <dfInputText class="form-control" ref="url.webpage" placeholder="Enter source webpage URL"/>
    <span class="help-block">
      <dfErrorList ref="url.webpage"/>
    </span>
  </div>

  <div class="form-group" >
    <dfLabel ref="url.remote" class="control-label required">And/Or Dataset URL</dfLabel>
    <dfInputText class="form-control" ref="url.remote" placeholder="Enter dataset URL"/>
    <span class="help-block">
      <dfErrorList ref="url.remote"/>
    </span>
  </div>

  <div class="form-group" >
    <dfLabel ref="url.local" class="control-label required">Or choose a local dataset file</dfLabel>
    <dfInputFile class="form-control" ref="url.local"/>
    <span class="help-block">
      <dfErrorList ref="url.local"/>
    </span>
  </div>

  <span class="help-block">
    <dfErrorList ref="url"/>
  </span>

  <div class="form-group">
    <dfInputSubmit value="${submitText}" class="btn btn-primary" data-disable-with="Submitting..."/>
  </div>
</dfForm>
