package main

import (
	"fmt"
)

type Fragment struct {
	starting  Position
	following Position
}

func NewFragment(starting, following Position) Fragment {
	return Fragment{starting: starting, following: following}
}

func (f Fragment) String() string {
	return fmt.Sprintf("%s-%s", f.starting.String(), f.following.String())
}
