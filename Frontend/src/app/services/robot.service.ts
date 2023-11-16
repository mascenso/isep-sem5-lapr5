import { Injectable } from '@angular/core';
import {environment} from "../../environments/environment";
import {HttpClient} from "@angular/common/http";
import {Observable} from "rxjs";

export interface RobotDto {
  id: string;
  nickName: string;
  robotType: string;
  serialNumber: string;
  description?: string;
  inhibited: boolean;
}

@Injectable({
  providedIn: 'root'
})
export class RobotService {

  private API_URL = environment.API_URL;

  constructor(private http: HttpClient) { }

  public findRobotsByTaskTypeOrDesignation(taskType: string | null, designation: string | null, showSpinner?:boolean): Observable<RobotDto[]> {
    const queryParams = {
      taskType: taskType ? taskType : '',
      designation: designation ? designation : ''
    };
    return this.http.get<RobotDto[]>(
      `${this.API_URL}/api/robots/search`, {
          params: {...queryParams},
        reportProgress: showSpinner
      });
  }
}
