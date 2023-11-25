import { Component, OnDestroy, OnInit } from "@angular/core";
import { MatSnackBar } from "@angular/material/snack-bar";
import { Observable, Subscription } from "rxjs";
import { BridgeService } from "../../../services/bridge.service";
import { BridgeDTO, BridgeFloorBuildingDTO } from "../../../../dto/bridgeDTO";
import { FloorService } from "../../../services/floor.service";
import { FloorResponseDTO } from "../../../../dto/floorDTO";
import { BuildingService } from "../../../services/building.service";
import { BuildingResponseDTO } from "../../../../dto/buildingDTO";

@Component({
  selector: 'app-bridge-list',
  templateUrl: './bridge-list.component.html',
  styleUrls: ['./bridge-list.component.css']
})
export class BridgeListComponent implements OnInit, OnDestroy {

  bridgeServiceSubscription$ = new Subscription();
  dataSource: BridgeFloorBuildingDTO[] = [];


  columnsToDisplay = ['code', 'name', 'floorNumberA', 'buildingNameA', 'floorNumberB', 'buildingNameB'];

  constructor(
              private bridgeService: BridgeService,
              private _snackBar: MatSnackBar) {
  }


  ngOnInit(): void {
    this.bridgeServiceSubscription$ = this.bridgeService.getAllBridges().subscribe(
      bridgeData => {
        this.dataSource = bridgeData;
      },
    )
  }

  ngOnDestroy(): void {
    this.bridgeServiceSubscription$.unsubscribe();
  }

}
