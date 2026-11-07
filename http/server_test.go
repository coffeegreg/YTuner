package http

import (
	"net/http"
	"net/http/httptest"
	"testing"
	"encoding/xml"
)

func TestHandleLoginXML(t *testing.T) {
	s := NewServer()
	req, _ := http.NewRequest("GET", "/setupapp/yamaha/loginxml.asp", nil)
	rr := httptest.NewRecorder()

	s.Router.ServeHTTP(rr, req)

	if rr.Code != http.StatusOK {
		t.Errorf("Expected status 200, got %d", rr.Code)
	}

	expected := xml.Header + `<ListOfItems><ItemCount>1</ItemCount><Item><ItemType>Dir</ItemType><Title>My Stations</Title><UrlDir>http:///viritin/mystations?viritin=true</UrlDir><UrlDirBackUp>http:///viritin/mystations?viritin=true</UrlDirBackUp><DirCount>0</DirCount></Item></ListOfItems>`
	if rr.Body.String() != expected {
		t.Errorf("Expected body %s, got %s", expected, rr.Body.String())
	}
}
