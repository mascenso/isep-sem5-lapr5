import { Injectable } from '@angular/core';
import {Observable} from "rxjs";
import {HttpClient} from "@angular/common/http";
import {environment} from "../../environments/environment";
import { BuildingResponseDTO, CreateBuildingRequestDTO } from "../../dto/buildingDTO";


@Injectable({
  providedIn: 'root'
})
export class BuildingService {

  private API_URL = environment.API_URL;

  constructor(private http: HttpClient) { }

  public createBuilding(building: CreateBuildingRequestDTO, showSpinner?: boolean): Observable<BuildingResponseDTO> {
    return this.http.post<BuildingResponseDTO>(`${this.API_URL}/api/buildings`, building, {reportProgress: showSpinner});
  }

  public getAllBuildings(): Observable<BuildingResponseDTO[]> {
    return this.http.get<BuildingResponseDTO[]>(`${this.API_URL}/api/buildings`);
  }

}
