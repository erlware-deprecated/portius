
-record(transition_spec, {
	  transition_id,
	  from_repo,
	  to_repo,
	  sign_type
	 }).

-record(app_spec, {name, version, transition_id, package_suffix}).
-record(release_spec, {name, version, transition_id, package_suffix}).
