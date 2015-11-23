<dfForm action="${action}" role="form" class="source" method="${method}">
  <span class="help-block">
    <dfErrorList ref=""/>
  </span>

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
    <dfLabel ref="url.webpage_url" class="control-label required">Webpage URL</dfLabel>
    <dfInputText class="form-control" ref="url.webpage_url" placeholder="Enter source webpage URL"/>
    <span class="help-block">
      <dfErrorList ref="url"/>
    </span>
    <span class="help-block">
      <dfErrorList ref="url.webpage_url"/>
    </span>
  </div>

  <div class="form-group" >
    <dfLabel ref="url.remote_dataset_url" class="control-label required">And/Or Dataset URL</dfLabel>
    <dfInputText class="form-control" ref="url.remote_dataset_url" placeholder="Enter dataset URL"/>
    <span class="help-block">
      <dfErrorList ref="url.remote_dataset_url"/>
    </span>
  </div>

  <div class="form-group" >
    <dfLabel ref="url.local_dataset_path" class="control-label required">Or choose a local dataset file</dfLabel>
    <dfInputFile class="form-control" ref="url.local_dataset_path"/>
    <span class="help-block">
      <dfErrorList ref="url.local_dataset_path"/>
    </span>
  </div>

  <div class="form-group">
    <dfInputSubmit value="${submitText}" class="btn btn-primary" data-disable-with="Submitting..."/>
  </div>
</dfForm>
