MONGO_CONTAINER=mongo1
MONGO_USER = root
MONGO_PASSWORD = password
MONGO_SLEEP = 3

.PHONY: run-env
run-env: .base-run-env .after-run-env

.base-run-env:
	docker-compose --project-name bigdata up -d --wait --build
.after-run-env: .init-replica

.PHONY: restart-env
restart-env: stop-env run-env

.PHONY: stop-env
stop-env:
	docker-compose --project-name bigdata down


.init-replica:
	@echo "Waiting for MongoDB to start..."
	@until docker exec -it mongo1 mongosh --eval "db.adminCommand('ping')" > /dev/null 2>&1; do sleep 2; done;

	@echo "Initializing replica set..."
	docker exec -it mongo1 mongosh \
		--username root --password password --authenticationDatabase admin \
		--eval "rs.initiate({ \
			_id: 'rs0', \
			members: [\
				{ _id: 0, host: 'mongo1:27017' },\
				{ _id: 1, host: 'mongo2:27017' },\
				{ _id: 2, host: 'mongo3:27017' }\
			]\
		})"

	@echo "Waiting for primary election..."
	@until docker exec -it mongo1 mongosh --quiet --username root --password password --authenticationDatabase admin --eval "rs.isMaster().ismaster" | grep true; do sleep 2; done

	@echo "Creating users..."
	docker exec -it mongo1 mongosh "mongodb://root:password@mongo1:27017,mongo2:27017,mongo3:27017/admin?replicaSet=rs0" --eval ' \
		db.getSiblingDB("test1").createUser({\
			user: "user",\
			pwd: "password",\
			roles: [{ role: "readWrite", db: "test1" }]\
		});\
	'
	@echo "Done."