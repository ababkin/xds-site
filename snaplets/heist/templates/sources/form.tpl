<form role="form" class="source" action="${postAction}" accept-charset="UTF-8" method="post">
  <div class="form-group">
    <label class="sr-only control-label required" for="source_title">Title</label>
    <input class="form-control form-control" placeholder="Enter source title" type="text" name="title" id="source_title">
  </div>

  <div class="form-group">
    <label class="sr-only control-label required" for="source_description">Description</label>
    <textarea class="form-control form-control" rows="3" placeholder="Enter brief description" name="description" id="source_description"></textarea>
  </div>

  <div class="form-group">
    <label class="sr-only control-label required" for="source_url">Title</label>
    <input class="form-control form-control" placeholder="Enter source URL" type="text" name="url" id="source_url">
  </div>

  <div class="form-group">
    <button name="button" type="submit" class="btn btn-primary" data-disable-with="Submitting..."><submitText/></button>
  </div>
</form>
