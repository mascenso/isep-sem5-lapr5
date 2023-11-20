import { Injectable } from '@angular/core';
import {environment} from "../../environments/environment";
import {HttpClient} from "@angular/common/http";
import {Observable} from "rxjs";
import { RobotDTO } from "../../dto/robotDTO";

@Injectable({
  providedIn: 'root'
})
export class RobotService {

  private API_URL = environment.API_URL;

  constructor(private http: HttpClient) { }

  public findRobotsByTaskTypeOrDesignation(taskType: string | null, designation: string | null, showSpinner?:boolean): Observable<RobotDTO[]> {
    const queryParams = {
      taskType: taskType ? taskType : '',
      designation: designation ? designation : ''
    };
    return this.http.get<RobotDTO[]>(
      `${this.API_URL}/api/robots/search`, {
          params: {...queryParams},
        reportProgress: showSpinner
      });
  }
}
