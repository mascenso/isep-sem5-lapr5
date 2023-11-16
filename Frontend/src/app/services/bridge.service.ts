import { Injectable } from '@angular/core';
import {Observable} from "rxjs";
import {HttpClient} from "@angular/common/http";
import {environment} from "../../environments/environment";

export interface BridgeResponseDto {
  id: string;
  name: string;
  code : string;
  floorAId : string;
  floorBId : string;
}

export interface CreateBridgeRequestDto {
  name: string;
  code : string;
  floorAId : string;
  floorBId : string;
}

export interface BridgeDto {
  name: string;
  code : string;
  floorAId : string;
  floorBId : string;
}

export interface BuildingResponseDto {
  id: string;
  code: string;
  maxWidth: string,
  maxLength: string,
  name: string;
  description: string;
}

export interface FloorResponseDto {
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
export class BridgeService {

  private API_URL = environment.API_URL;

  constructor(private http: HttpClient) { }

  public createBridge(bridge: CreateBridgeRequestDto, showSpinner?: boolean): Observable<BridgeResponseDto> {
    console.log(bridge);
    return this.http.post<BridgeResponseDto>(`${this.API_URL}/api/bridges`, bridge, {reportProgress: showSpinner});
  }

  public getAllBridges(): Observable<BridgeResponseDto[]> {
    return this.http.get<BridgeResponseDto[]>(`${this.API_URL}/api/bridges`);
  }

  getAllBuildings() {
    return this.http.get<BuildingResponseDto[]>(`${this.API_URL}/api/buildings`);
  }

  getFloorsByBuildingId($event: any, b: boolean) {
    return this.http.get<FloorResponseDto[]>(`${this.API_URL}/api/floors/buildings?building=${$event}`, {reportProgress: b});
  }
}
