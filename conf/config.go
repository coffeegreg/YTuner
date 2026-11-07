package conf

import (
	"gopkg.in/ini.v1"
	"gopkg.in/yaml.v3"
	"os"
)

type ViritinConfig struct {
	WebServerPort         int      `ini:"WebServerPort"`
	InterceptDNs          []string `ini:"InterceptDNs"`
	DNSAdvertiseIP        string   `ini:"DNSAdvertiseIP"`
	DNSRestrictForwarding bool     `ini:"DNSRestrictForwarding"`
	CommonAVRini          bool     `ini:"CommonAVRini"`
}

type RetunerConfig = ViritinConfig

func LoadViritinConfig(path string) (*ViritinConfig, error) {
	cfg, err := ini.Load(path)
	if err != nil {
		return nil, err
	}

	viritinCfg := &ViritinConfig{
		WebServerPort:         80,
		CommonAVRini:          true,
	}

	section := cfg.Section("Viritin")
	if !section.HasKey("WebServerPort") {
		section = cfg.Section("Retuner")
	}
	if !section.HasKey("WebServerPort") {
		section = cfg.Section("Configuration")
	}
	viritinCfg.WebServerPort = section.Key("WebServerPort").MustInt(80)
	viritinCfg.InterceptDNs = section.Key("InterceptDNs").Strings(",")
	viritinCfg.DNSAdvertiseIP = section.Key("DNSAdvertiseIP").String()
	viritinCfg.DNSRestrictForwarding = section.Key("RestrictForwarding").MustBool(false)
	viritinCfg.CommonAVRini = section.Key("CommonAVRini").MustBool(true)

	return viritinCfg, nil
}

func LoadRetunerConfig(path string) (*RetunerConfig, error) {
	return LoadViritinConfig(path)
}

type Station struct {
	URL  string
	Logo string
}

type StationsConfig map[string]map[string]Station

type YAMLStation string
type YAMLStations map[string]map[string]YAMLStation

func LoadYAMLStations(path string) (StationsConfig, error) {
	data, err := os.ReadFile(path)
	if err != nil {
		return nil, err
	}

	var yamlCfg YAMLStations
	if err := yaml.Unmarshal(data, &yamlCfg); err != nil {
		return nil, err
	}

	cfg := make(StationsConfig)
	for catName, cat := range yamlCfg {
		cfg[catName] = make(map[string]Station)
		for statName, statData := range cat {
			parts := []rune(statData)
			url := ""
			logo := ""
			pipeIdx := -1
			for i, r := range parts {
				if r == '|' {
					pipeIdx = i
					break
				}
			}
			if pipeIdx != -1 {
				url = string(parts[:pipeIdx])
				logo = string(parts[pipeIdx+1:])
			} else {
				url = string(statData)
			}
			cfg[catName][statName] = Station{URL: url, Logo: logo}
		}
	}
	return cfg, nil
}
