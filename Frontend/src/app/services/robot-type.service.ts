import { Injectable } from '@angular/core';
import { HttpClient } from '@angular/common/http';
import {Observable, Subject} from 'rxjs';
import {environment} from "../../environments/environment";
import { CreateRobotTypeRequestDTO, RobotTypeResponseDTO } from "../../dto/robotTypeDTO";

@Injectable({
  providedIn: "root"
})
export class RobotTypeService {

  private API_URL = environment.API_URL;

  constructor(private http: HttpClient) {
  }

  public createRobotType(robotType: CreateRobotTypeRequestDTO): Observable<RobotTypeResponseDTO> {
    return this.http.post<RobotTypeResponseDTO>(`${this.API_URL}/api/robots/types`, robotType);
  }
}
