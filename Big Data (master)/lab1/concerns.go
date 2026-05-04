package main

// (из документации https://mongo-db.ru/core/transactions/index.html?ysclid=mm0e6edwqf369699603#transactions-and-read-concern)
type ReadConcern string

const (
	// ReadConcernLocal returns the most recent data available from the node but can be rolled back.
	//
	// On a replica set, even if a transaction uses read concern local, you might observe stronger read
	// isolation where the operation reads from a snapshot at the point in time that the transaction
	// was opened.
	//
	// For transactions on sharded cluster, "local" read concern cannot guarantee that the data is from
	// the same snapshot view across the shards. If snapshot isolation is required,
	// use "snapshot" read concern.
	ReadConcernLocal ReadConcern = "local"
	// ReadConcernMajority If the transaction commits with write concern "majority", read concern "majority" returns
	// data that has been acknowledged by a majority of the replica set members and can't be rolled back.
	// Otherwise, read concern "majority" provides no guarantees that read operations
	// read majority-committed data.
	//
	// For transactions on sharded cluster, read concern "majority" can't guarantee that the data
	// is from the same snapshot view across the shards. If snapshot isolation is required, use read
	// concern "snapshot".
	ReadConcernMajority ReadConcern = "majority"
	// ReadConcernSnapshot returns data from a snapshot of majority committed data if the
	// transaction commits with write concern "majority".
	//
	// If the transaction does not use write concern "majority" for the commit, the "snapshot"
	// read concern provides no guarantee that read operations used a snapshot of majority-committed data.
	//
	// For transactions on sharded clusters, the "snapshot" view of the data is synchronized across shards.
	ReadConcernSnapshot ReadConcern = "snapshot"
)

// Transactions use the transaction-level write concern to commit the write operations.
// Write operations inside transactions must be run without an explicit write concern
// specification and use the default write concern.
// At commit time, the writes committed using the transaction-level write concern.
type WriteConcern string

const (
	// Write concern w: 1 returns acknowledgment after the commit has been applied to the
	// primary.
	//
	// IMPORTANT
	// When you commit with w: 1, your transaction can be rolled back if there is a failover.
	//
	// When you commit with w: 1 write concern, transaction-level "majority" read concern
	// provides no guarantees that read operations in the transaction read majority-committed
	// data.
	//
	// When you commit with w: 1 write concern, transaction-level "snapshot" read concern
	// provides no guarantee that read operations in the transaction used a snapshot
	// of majority-committed data.
	WriteConcernOne = "1"
	// Write concern w: "majority" returns acknowledgment after the commit has been applied
	// to a majority of voting members.
	//
	// When you commit with w: "majority" write concern, transaction-level "majority" read
	// concern guarantees that operations have read majority-committed data.
	// For transactions on sharded clusters, this view of the majority-committed data
	// is not synchronized across shards.
	//
	// When you commit with w: "majority" write concern, transaction-level "snapshot"
	// read concern guarantees that operations have read from a synchronized snapshot
	// of majority-committed data.
	WriteConcernMajority = "majority"
)
