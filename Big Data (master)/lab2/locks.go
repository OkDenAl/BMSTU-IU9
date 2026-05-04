package kvtransaction

import "sync"

type lockManager struct {
	mu    sync.RWMutex
	locks map[string]string
}

func newLockManager() *lockManager {
	return &lockManager{
		locks: make(map[string]string),
	}
}

func (lm *lockManager) tryLock(txID, key string) bool {
	lm.mu.Lock()
	defer lm.mu.Unlock()

	holder, ok := lm.locks[key]
	if ok && holder != txID {
		return false
	}

	lm.locks[key] = txID
	return true
}

func (lm *lockManager) unlock(txID, key string) {
	lm.mu.Lock()
	defer lm.mu.Unlock()

	holder, ok := lm.locks[key]
	if ok && holder == txID {
		delete(lm.locks, key)
	}
}
