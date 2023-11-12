import { Injectable } from '@angular/core';
import { HttpClient } from '@angular/common/http';
import {Observable, Subject} from 'rxjs';
import {environment} from "../../environments/environment";

export interface RobotTypeResponseDto {
  id: string;
  designacao: string;
  tipoTarefas: string[],
}

export interface CreateRobotTypeRequestDto {
  designacao: string;
  TipoTarefas: string[],
}

@Injectable({
  providedIn: 'root'
})
export class RobotTypeService {

  private API_URL = environment.API_URL;

  constructor(private http: HttpClient) { }

  public createRobotType(robotType: CreateRobotTypeRequestDto): Observable<RobotTypeResponseDto> {
    return this.http.post<RobotTypeResponseDto>(`${this.API_URL}/api/robots/types`, robotType);
  }
}
