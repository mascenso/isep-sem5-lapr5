import { Injectable } from '@angular/core';
import {Observable} from "rxjs";
import {HttpClient} from "@angular/common/http";
import {environment} from "../../environments/environment";

export interface BuildingResponseDto {
  id: string;
  code: string;
  maxWidth: string,
  maxLength: string,
  name: string;
  description: string;
}

export interface CreateBuildingRequestDto {
  code: string;
  maxWidth: string,
  maxLength: string,
  name?: string;
  description?: string;
}


@Injectable({
  providedIn: 'root'
})
export class BuildingService {

  private API_URL = environment.API_URL;

  constructor(private http: HttpClient) { }

  public createBuilding(building: CreateBuildingRequestDto, showSpinner?: boolean): Observable<BuildingResponseDto> {
    return this.http.post<BuildingResponseDto>(`${this.API_URL}/api/buildings`, building, {reportProgress: showSpinner});
  }

  public getAllBuildings(): Observable<BuildingResponseDto[]> {
    return this.http.get<BuildingResponseDto[]>(`${this.API_URL}/api/buildings`);
  }

}
