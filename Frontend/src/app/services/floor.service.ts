import { Injectable } from '@angular/core';
import {environment} from "../../environments/environment";
import {HttpClient} from "@angular/common/http";
import {Observable} from "rxjs";

export interface FloorDto {
  id: string;
  buildingId:string;
  width: number;
  length: number;
  floorNumber: number;
  description: string;
  floorMap: number[][];
}

@Injectable({
  providedIn: 'root'
})
export class FloorService {

  private API_URL = environment.API_URL;

  constructor(private http: HttpClient) { }

  public getFloorsWithElevatorByBuildingId(buildingId: string, showSpinner?:boolean): Observable<FloorDto[]> {
    return this.http.get<FloorDto[]>(`${this.API_URL}/api/floors/buildings/${buildingId}/with-elevator`, {reportProgress: showSpinner});
  }

}
