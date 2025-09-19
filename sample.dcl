template lambda_fn
  meta
    always add endpoint-url
  create
    aws create-function
    aws list-functions
  update
  delete

resource lambda_fn(my_lambda_fn)
  {}
