# CalLang runtime

# Takes the created json file and calls the google calendar api on it,
# creating the calendar events.

import json
import sys
from google.oauth2.credentials import Credentials
from google_auth_oauthlib.flow import InstalledAppFlow
from googleapiclient.discovery import build
from googleapiclient.errors import HttpError

def get_calendar_service(credentials_file_name):
    flow = InstalledAppFlow.from_client_secrets_file(
        credentials_file_name, 
        ['https://www.googleapis.com/auth/calendar']
    )
    creds = flow.run_local_server(port=0)
    return build('calendar', 'v3', credentials=creds)

def load_events(json_file):
    with open(json_file) as f:
        return json.load(f)

def create_calendar_events(service, events):
    for event in events:
        try:
            service.events().insert(calendarId='primary', body=event).execute()
        except HttpError as error:
            print(f"Failed to create {event.get('summary', 'unnamed event')}: {error}")
        except KeyError as e:
            print(f"Missing required field {e} in event: {event.get('summary', 'unnamed event')}")
    

if __name__ == '__main__':
    json_file_name = sys.argv[1]
    creds_file_name = sys.argv[2]
    service = get_calendar_service(creds_file_name)
    events = load_events(json_file_name)
    create_calendar_events(service, events)