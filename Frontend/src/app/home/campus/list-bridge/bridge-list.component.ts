import { Component, OnDestroy, OnInit } from "@angular/core";
import { MatSnackBar } from "@angular/material/snack-bar";
import { Observable, Subscription } from "rxjs";
import { BridgeService } from "../../../services/bridge.service";
import { BridgeResponseDTO } from "../../../../dto/bridgeDTO";
import { BuildingResponseDTO } from "../../../../dto/buildingDTO";
import { FormControl } from "@angular/forms";

@Component({
  selector: 'app-bridge-list',
  templateUrl: './bridge-list.component.html',
  styleUrls: ['./bridge-list.component.css']
})
export class BridgeListComponent implements OnInit, OnDestroy {

  bridgeServiceSubscription$ = new Subscription();
  dataSource: BridgeResponseDTO[] = [];

  columnsToDisplay = ['name', 'floorANumber', 'buildingAName', 'floorBNumber', 'buildingBName'];

  buildingList: BuildingResponseDTO[] = [];
  buildingBList: BuildingResponseDTO[] = [];

  buildingAId: string = '';
  buildingBId: string = '';

  buildingSelectionControl = new FormControl();
  buildingBSelectionControl = new FormControl();

  buildingServiceSubscription$ = new Subscription();

  constructor(
              private bridgeService: BridgeService,
              private _snackBar: MatSnackBar) {
  }


  ngOnInit(): void {

    this.buildingServiceSubscription$ = this.bridgeService.getAllBuildings().subscribe(
      response => {
        this.buildingList = response;
        this.buildingBList = response;
      },

    )

    this.bridgeServiceSubscription$ = this.bridgeService.getAllBridges().subscribe(
      bridgeData => {
        this.dataSource = bridgeData;
      },
    );
  }

  ngOnDestroy(): void {
    this.bridgeServiceSubscription$.unsubscribe();
  }

  onSelectionAUpdateTable($event: any) {

    this.buildingAId = $event;

    if ((this.buildingAId != '' && this.buildingBId != '')) {
      this.bridgeServiceSubscription$ = this.bridgeService.getBridgesBetweenBuildings(this.buildingAId, this.buildingBId).subscribe(
        bridgeData => {
          this.dataSource = bridgeData;
        },
      );
    }
    else
    {
      this.bridgeServiceSubscription$ = this.bridgeService.getAllBridges().subscribe(
        bridgeData => {
          this.dataSource = bridgeData;
        },
      );
    }

  }

  onSelectionBUpdateTable($event: any) {
    this.buildingBId = $event;

    console.log(this.buildingAId);
    console.log(this.buildingBId);

    if (this.buildingAId != '' && this.buildingBId != '') {
      this.bridgeServiceSubscription$ = this.bridgeService.getBridgesBetweenBuildings(this.buildingAId, this.buildingBId).subscribe(
        bridgeData => {
          this.dataSource = bridgeData;
        },
      );
    }
    else
    {
      this.bridgeServiceSubscription$ = this.bridgeService.getAllBridges().subscribe(
        bridgeData => {
          this.dataSource = bridgeData;
        },
      );
    }
  }
}
