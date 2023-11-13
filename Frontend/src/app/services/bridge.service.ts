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

@Injectable({
  providedIn: 'root'
})
export class BridgeService {

  private API_URL = environment.API_URL;

  constructor(private http: HttpClient) { }

  public createBridge(bridge: CreateBridgeRequestDto, showSpinner?: boolean): Observable<BridgeResponseDto> {
    return this.http.post<BridgeResponseDto>(`${this.API_URL}/api/bridges`, bridge, {reportProgress: showSpinner});
  }

  public getAllBridges(): Observable<BridgeResponseDto[]> {
    return this.http.get<BridgeResponseDto[]>(`${this.API_URL}/api/bridges`);
  }

}
