# vs-migrate
Scripts for migrating data from formhub to the Vital Signs database

To run this script, first set up an ssh tunnel to either the formhub server or a local test server.

A VM image of an ubuntu server with a test database and the connection strings are in the S3 bucket vs-migrate. The username is `ubuntu` and the password is `ubuntu`. Once the local VM server is set up, refresh the database to the latest copy of the production database using `./reload_database.sh`.