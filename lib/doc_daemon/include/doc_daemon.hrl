
-record(doc_spec, {
	  transition_id,
	  repo_dir_path,
	  webserver_doc_root,
	  generated_docs_base_dir,
	  app_index_file_src,
	  app_index_file,
	  release_index_file_src,
	  release_index_file,
	  no_doc_list
	 }).

-record(app_spec, {name, version, transition_id, package_path, doc_path, erts_vsn}).
-record(release_spec, {name, version, transition_id, package_path, doc_path, erts_vsn}).


		   
