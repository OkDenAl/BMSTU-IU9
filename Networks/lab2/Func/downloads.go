package Func

import (
	"github.com/mgutz/logxi/v1"
	"golang.org/x/net/html"
	"net/http"
)

func getAttr(node *html.Node, key string) string {
	for _, attr := range node.Attr {
		if attr.Key == key {
			return attr.Val
		}
	}
	return ""
}

func getChildren(node *html.Node) []*html.Node {
	var children []*html.Node
	for c := node.FirstChild; c != nil; c = c.NextSibling {
		children = append(children, c)
	}
	return children
}

func isElem(node *html.Node, tag string) bool {
	return node != nil && node.Type == html.ElementNode && node.Data == tag
}

func isText(node *html.Node) bool {
	return node != nil && node.Type == html.TextNode
}

func isDiv(node *html.Node, class string) bool {
	return isElem(node, "div") && getAttr(node, "class") == class
}

func isPicture(node *html.Node, class string) bool {
	return isElem(node, "picture") && getAttr(node, "class") == class
}

func readItem(item *html.Node) *Item {
	if a := item.FirstChild; isElem(a, "a") {
		cs := getChildren(a.FirstChild)
		if len(cs) == 1 && isText(cs[0]) {
			return &Item{
				Ref:      getAttr(a, "href"),
				Title:    cs[0].Data,
				ImageSrc: imageSrc,
			}
		}
	}
	return nil
}

var items map[string]*Item
var imageSrc string

func searchFilmsNames(node *html.Node) map[string]*Item {
	if isPicture(node, "_3bgWH _3CcIM HaAVD") {
		imageSrc = getAttr(node.FirstChild, "src")
	}
	if isDiv(node, "_1V-Pk") {
		for c := node.FirstChild; c != nil; c = c.NextSibling {
			if item := readItem(c); item != nil {
				items[item.Title] = item
			}
		}
		return items
	}
	for c := node.FirstChild; c != nil; c = c.NextSibling {
		if items1 := searchFilmsNames(c); items1 != nil && c.NextSibling == nil {
			return items1
		}
	}
	return nil
}

type Item struct {
	Ref, Title, ImageSrc string
}

func DownloadNews() map[string]*Item {
	log.Info("sending request to www.afisha.ru")
	if response, err := http.Get("https://www.afisha.ru/msk/cinema"); err != nil {
		log.Error("request to www.afusha.ru failed", "error", err)
	} else {
		defer response.Body.Close()
		status := response.StatusCode
		log.Info("got response from www.afisha.ru", "status", status)
		if status == http.StatusOK {
			if data, err := html.Parse(response.Body); err != nil {
				log.Error("invalid HTML from www.afisha.ru", "error", err)
			} else {
				log.Info("HTML from www.afisha.ru parsed successfully")
				items = make(map[string]*Item)
				searchFilmsNames(data)
				return items
			}
		}
	}
	return nil
}
