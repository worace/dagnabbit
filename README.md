## Dagnabbit

Some DAGs in Scala

### TODO

* Composable targets? Could there be a flatMap-based API for target checks, so that you could have deps for a target easily to prevent crashes for checks that will fail without their prereqs...e.g. a DB server must exist before a schema must exist before a table must exist, etc. `schemaTarget >> tableTarget >> ...`
