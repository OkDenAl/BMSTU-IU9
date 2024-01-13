package proto

import "encoding/json"

// Request -- запрос клиента к серверу.
type Request struct {
	// Поле Command может принимать три значения:
	// * "quit" - прощание с сервером (после этого сервер рвёт соединение);
	// * "add" - передача новой дроби на сервер;
	// * "avg" - просьба посчитать среднее арифметическое всех переданных дробей.
	Command string `json:"command"`
	// Если Command == "add", в поле Data должна лежать дробь
	// в виде структуры Fraction.
	// В противном случае, поле Data пустое.
	Data *json.RawMessage `json:"data"`
}

// Response -- ответ сервера клиенту.
type Response struct {
	// Поле Status может принимать три значения:
	// * "ok" - успешное выполнение команды "quit" или "add";
	// * "failed" - в процессе выполнения команды произошла ошибка;
	// * "count" - количество слов вычислено.
	Status string `json:"status"`

	// Если Status == "failed", то в поле Data находится сообщение об ошибке.
	// Если Status == "result", в поле Data должно лежать число
	// в виде структуры Fraction.
	// В противном случае, поле Data пустое.
	Data *json.RawMessage `json:"data"`
}

type Fraction struct {
	Arr   string `json:"a"`
	Count string `json:"count"`
}
