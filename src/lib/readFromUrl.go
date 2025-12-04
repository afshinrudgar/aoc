package lib

import "net/http"

func ReadFromUrl(url string) (string, error) {
	resp, err := http.Get(url)
	if err != nil {
		return "", err
	}
	defer resp.Body.Close()

	buf := make([]byte, 0)
	for {
		tmp := make([]byte, 1024)
		n, err := resp.Body.Read(tmp)
		if err != nil {
			break
		}
		buf = append(buf, tmp[:n]...)
	}

	return string(buf), nil
}
