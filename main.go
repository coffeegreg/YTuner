package main

import (
	"flag"
	"log"
	"viritin/http"
	"viritin/dns"
	"viritin/conf"
	"os"
	"os/signal"
	"syscall"
)

func main() {
	var port int
	var configPath string
	flag.IntVar(&port, "p", 0, "Web server port")
	flag.StringVar(&configPath, "c", "viritin.ini", "Path to configuration file")
	flag.Parse()

	cfg, err := conf.LoadViritinConfig(configPath)
	if err != nil {
		log.Printf("Warning: failed to load config %s: %v, using defaults", configPath, err)
		cfg = &conf.ViritinConfig{
			WebServerPort: 80,
			InterceptDNs:  []string{"*.vtuner.com"},
			DNSAdvertiseIP: "127.0.0.1",
		}
	}

	if port != 0 {
		cfg.WebServerPort = port
	}

	if cfg.DNSAdvertiseIP == "" {
		cfg.DNSAdvertiseIP = "127.0.0.1" // Fallback to avoid DNS failing to answer
	}

	// Start DNS Server (ignore failures for tests that run without privileges)
	dnsServer := dns.NewServer(cfg.InterceptDNs, cfg.DNSAdvertiseIP)
	go func() {
		log.Printf("Starting DNS Server on :53")
		if err := dnsServer.Start(53); err != nil {
			log.Printf("Warning: DNS server failed to start: %v", err)
		}
	}()

	// Start HTTP Server
	httpServer := http.NewServer()
	go func() {
		log.Printf("Starting HTTP Server on :%d", cfg.WebServerPort)
		if err := httpServer.Start(cfg.WebServerPort); err != nil && err.Error() != "http: Server closed" {
			log.Fatalf("HTTP server failed: %v", err)
		}
	}()

	// Wait for termination signal
	sig := make(chan os.Signal, 1)
	signal.Notify(sig, syscall.SIGINT, syscall.SIGTERM)
	<-sig
}
