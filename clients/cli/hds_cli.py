"""Command line client for Health Data Science API."""
import argparse
import requests

parser = argparse.ArgumentParser()
parser.add_argument('--base-url', required=True)
parser.add_argument('--api-key')
sub = parser.add_subparsers(dest='cmd')
sub.add_parser('health')

args = parser.parse_args()
headers = {'X-API-Key': args.api_key} if args.api_key else {}

if args.cmd == 'health':
    r = requests.get(f"{args.base_url.rstrip('/')}/v1/health/live", headers=headers)
    print(r.text)
