package main

import (
	"fmt"
)

func encode(utf32 []rune) []byte {
	res := make([]byte, 0)
	var mask byte
	var curSym byte
	for _, sym := range utf32 {
		mask = 255
		if sym < 1<<7 {
			res = append(res, byte(sym))
		} else if sym < 1<<11 {
			curSym = (byte(sym>>6) | mask<<6) & (mask<<6 | mask>>3)
			res = append(res, curSym)
			curSym = (byte(sym) | mask<<7) & (mask<<7 | mask>>2)
			res = append(res, curSym)
		} else if sym < 1<<16 {
			curSym = (byte(sym>>12) | mask<<5) & (mask<<5 | mask>>4)
			res = append(res, curSym)
			curSym = (byte(sym>>6) | mask<<7) & (mask<<7 | mask>>2)
			res = append(res, curSym)
			curSym = (byte(sym) | mask<<7) & (mask<<7 | mask>>2)
			res = append(res, curSym)
		} else {
			res = append(res, (byte(sym>>18)|mask<<4)&247)
			res = append(res, (byte(sym>>12)|mask<<7)&(mask<<7|mask>>2))
			res = append(res, (byte(sym>>6)|mask<<7)&(mask<<7|mask>>2))
			res = append(res, (byte(sym)|mask<<7)&(mask<<7|mask>>2))
		}
	}
	return res
}

func decode(utf8 []byte) []rune {
	res := make([]rune, 0)
	var mask byte
	i := 0
	for i < len(utf8) {
		mask = 255
		if utf8[i] < 1<<7 {
			res = append(res, rune(int(utf8[i])))
			i++
		} else if utf8[i] <= (mask<<6|mask>>3) && utf8[i] >= (mask<<6) {
			//fmt.Printf("%b", (rune(utf8[i]&(mask<<8|mask>>3))<<6)|rune((utf8[i+1])&(mask<<8|mask>>3)))
			res = append(res, (rune(utf8[i]&(mask<<9|mask>>3))<<6)|rune((utf8[i+1])&(mask<<8|mask>>2)))
			i += 2
		} else if utf8[i] <= (mask<<5|mask>>4) && utf8[i] >= (mask<<5) {
			res = append(res, (rune(utf8[i]&(mask<<8|mask>>4))<<12)|
				rune((utf8[i+1])&(mask<<8|mask>>2))<<6|rune((utf8[i+2])&(mask<<8|mask>>2)))
			i += 3
		} else {
			res = append(res, (rune(utf8[i]&(mask<<8|mask>>5))<<14)|
				rune((utf8[i+1])&(mask<<8|mask>>2))<<12|rune((utf8[i+2])&(mask<<8|mask>>2))<<6|
				rune((utf8[i+3])&(mask<<8|mask>>2)))
			i += 4
		}
	}
	return res
}

func main() {
	var utf32 []rune = ([]rune)("M")
	res := encode(utf32)
	fmt.Println(utf32)
	fmt.Println(res)
	var utf8 []byte = ([]byte)("M")
	fmt.Println(utf32)
	res2 := decode(utf8)
	fmt.Println(res2)

}
