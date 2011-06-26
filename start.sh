#!/bin/bash
erl -pa ./ebin -s dreambook_logger -s dreambook_db_server -s dreambook_http_server
