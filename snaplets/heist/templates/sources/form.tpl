<dfForm action="/sources" role="form" class="source">
  <dfChildErrorList/>

  <div class="form-group">
    <dfLabel ref="title" class="control-label required">Title: </dfLabel>
    <dfInputText class="form-control" ref="title" placeholder="Enter source title"/>
  </div>

  <div class="form-group">
    <dfLabel ref="description" class="control-label">Description: </dfLabel>
    <dfInputTextArea class="form-control" ref="description" cols="20" rows="3" placeholder="Enter brief description"/>
  </div>

  <div class="form-group" >
    <dfLabel ref="url" class="control-label required">Url: </dfLabel>
    <dfInputText class="form-control" ref="url" placeholder="Enter source URL"/>
  </div>

  <div class="form-group">
    <dfInputSubmit value="${submitText}" class="btn btn-primary" data-disable-with="Submitting..."/>
  </div>
</dfForm>
