package kvtransaction

type msgType int

const (
	prepare msgType = iota
	commit
	rollback
)

type message struct {
	txID  string
	mType msgType
	kv    []any
	resp  chan bool
}

func newMessage(txID string, mType msgType, kv []any, resp chan bool) *message {
	return &message{
		txID:  txID,
		mType: mType,
		kv:    kv,
		resp:  resp,
	}
}
