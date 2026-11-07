package http

import (
	"fmt"
	"net/http"
	"viritin/vtuner"
	"encoding/xml"

	"github.com/go-chi/chi/v5"
)

type Server struct {
	Router *chi.Mux
}

func NewServer() *Server {
	s := &Server{
		Router: chi.NewRouter(),
	}
	s.routes()
	return s
}

func (s *Server) routes() {
	s.Router.Get("/", s.handleDefault)
	s.Router.Get("/setupapp/{vendor}/loginxml.asp", s.handleLoginXML)
	s.Router.Get("/setupapp/{vendor}/navxml.asp", s.handleNavXML)
	s.Router.Get("/setupapp/{vendor}/statxml.asp", s.handleStatXML)
	s.Router.Get("/setupapp/favxml.asp", s.handleFavXML)

	s.Router.Get("/viritin/mystations", s.handleMyStations)
	s.Router.Get("/retuner/mystations", s.handleMyStations)
	s.Router.Get("/ytuner/mystations", s.handleMyStations)
}

func (s *Server) handleDefault(w http.ResponseWriter, r *http.Request) {
	w.WriteHeader(http.StatusOK)
	w.Write([]byte("Viritin HTTP Server"))
}

func (s *Server) handleLoginXML(w http.ResponseWriter, r *http.Request) {
	if r.URL.Query().Get("token") == "0" {
		w.Header().Set("Content-Type", "text/xml")
		w.Write([]byte(xml.Header + "<EncryptedToken>0123456789ABCDEF</EncryptedToken>"))
		return
	}

	page := &vtuner.Page{}
	page.Add(vtuner.Directory{
		Title: "My Stations",
		UrlDir: vtuner.AddViritinParameter("http://" + r.Host + "/viritin/mystations"),
		UrlDirBackUp: vtuner.AddViritinParameter("http://" + r.Host + "/viritin/mystations"),
	})

	xmlStr, err := page.ToXMLString()
	if err != nil {
		http.Error(w, err.Error(), http.StatusInternalServerError)
		return
	}

	w.Header().Set("Content-Type", "text/xml")
	w.Write([]byte(xmlStr))
}

func (s *Server) handleNavXML(w http.ResponseWriter, r *http.Request) {
	s.handleLoginXML(w, r)
}

func (s *Server) handleStatXML(w http.ResponseWriter, r *http.Request) {
	w.WriteHeader(http.StatusNotImplemented)
}

func (s *Server) handleFavXML(w http.ResponseWriter, r *http.Request) {
	w.WriteHeader(http.StatusNotImplemented)
}

func (s *Server) handleMyStations(w http.ResponseWriter, r *http.Request) {
	page := &vtuner.Page{}

	xmlStr, err := page.ToXMLString()
	if err != nil {
		http.Error(w, err.Error(), http.StatusInternalServerError)
		return
	}

	w.Header().Set("Content-Type", "text/xml")
	w.Write([]byte(xmlStr))
}

func (s *Server) Start(port int) error {
	return http.ListenAndServe(fmt.Sprintf(":%d", port), s.Router)
}
