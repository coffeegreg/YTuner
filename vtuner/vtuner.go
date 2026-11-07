package vtuner

import (
	"encoding/xml"
	"strings"
)

type ItemType string

const (
	ItemTypeDir     ItemType = "Dir"
	ItemTypeStation ItemType = "Station"
	ItemTypeDisplay ItemType = "Display"
)

type Item interface {
	itemType() ItemType
}

type Directory struct {
	XMLName      xml.Name `xml:"Item"`
	ItemType     ItemType `xml:"ItemType"`
	Title        string   `xml:"Title"`
	UrlDir       string   `xml:"UrlDir"`
	UrlDirBackUp string   `xml:"UrlDirBackUp"`
	DirCount     int      `xml:"DirCount"`
}

func (d Directory) itemType() ItemType { return ItemTypeDir }

type Station struct {
	XMLName          xml.Name `xml:"Item"`
	ItemType         ItemType `xml:"ItemType"`
	StationId        string   `xml:"StationId"`
	StationName      string   `xml:"StationName"`
	StationUrl       string   `xml:"StationUrl"`
	StationDesc      string   `xml:"StationDesc"`
	Logo             string   `xml:"Logo"`
	StationFormat    string   `xml:"StationFormat"`
	StationLocation  string   `xml:"StationLocation"`
	StationBandWidth string   `xml:"StationBandWidth"`
	StationMime      string   `xml:"StationMime"`
	Relia            string   `xml:"Relia"`
	Bookmark         string   `xml:"Bookmark"`
}

func (s Station) itemType() ItemType { return ItemTypeStation }

type Display struct {
	XMLName  xml.Name `xml:"Item"`
	ItemType ItemType `xml:"ItemType"`
	Message  string   `xml:"Display"`
}

func (d Display) itemType() ItemType { return ItemTypeDisplay }

type ListOfItems struct {
	XMLName   xml.Name `xml:"ListOfItems"`
	ItemCount int      `xml:"ItemCount"`
	Items     []any    `xml:",any"`
}

type Page struct {
	TotalItemsCount int
	Items           []Item
}

func (p *Page) Add(item Item) {
	p.Items = append(p.Items, item)
	p.TotalItemsCount++
}

func (p *Page) ToXMLString() (string, error) {
	list := ListOfItems{
		ItemCount: p.TotalItemsCount,
	}

	for _, item := range p.Items {
		switch item.(type) {
		case Directory, *Directory:
			var dir Directory
			if ptr, ok := item.(*Directory); ok {
				dir = *ptr
			} else {
				dir = item.(Directory)
			}
			dir.ItemType = ItemTypeDir
			list.Items = append(list.Items, dir)
		case Station, *Station:
			var stat Station
			if ptr, ok := item.(*Station); ok {
				stat = *ptr
			} else {
				stat = item.(Station)
			}
			stat.ItemType = ItemTypeStation
			list.Items = append(list.Items, stat)
		case Display, *Display:
			var disp Display
			if ptr, ok := item.(*Display); ok {
				disp = *ptr
			} else {
				disp = item.(Display)
			}
			disp.ItemType = ItemTypeDisplay
			list.Items = append(list.Items, disp)
		}
	}

	bytes, err := xml.Marshal(list)
	if err != nil {
		return "", err
	}
	return xml.Header + string(bytes), nil
}

func AddViritinParameter(url string) string {
	if strings.Contains(url, "?") {
		return url + "&viritin=true"
	}
	return url + "?viritin=true"
}

func AddRetunerParameter(url string) string {
	return AddViritinParameter(url)
}
