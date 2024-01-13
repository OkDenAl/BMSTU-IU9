package entity

type User struct {
	UserId   int    `json:"userId"`
	Email    string `json:"email"`
	Password string `json:"password"`
}

type UserData struct {
	User   User  `json:"user"`
	Tokens Token `json:"tokens"`
}
