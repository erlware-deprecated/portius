
-record(transition_spec, {
	  transition_id,
	  from_repo,
	  to_repo,
	  children
	 }).

-record(doc_spec, {
	  webserver_doc_root,
	  generated_docs_base_dir,
	  app_index_file_src,
	  app_index_file,
	  release_index_file_src,
	  release_index_file
	 }).
