<dfForm action="${action}" role="form" class="source" method="${method}">

  <div class="form-group">
    <dfLabel ref="title" class="control-label required">Title</dfLabel>
    <dfInputText class="form-control" ref="title" placeholder="Enter source title"/>
    <span class="help-block">
      <dfErrorList ref="title"/>
    </span>
  </div>

  <div class="form-group">
    <dfLabel ref="description" class="control-label">Description</dfLabel>
    <dfInputTextArea class="form-control" ref="description" cols="20" rows="3" placeholder="Enter brief description"/>
    <span class="help-block">
      <dfErrorList ref="description"/>
    </span>
  </div>

  <div class="form-group" >
    <dfLabel ref="url" class="control-label required">Url</dfLabel>
    <dfInputText class="form-control" ref="url" placeholder="Enter source URL"/>
    <span class="help-block">
      <dfErrorList ref="url"/>
    </span>
  </div>

  <div class="form-group">
    <dfInputSubmit value="${submitText}" class="btn btn-primary" data-disable-with="Submitting..."/>
  </div>
</dfForm>
