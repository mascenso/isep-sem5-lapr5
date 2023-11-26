import { Component, OnDestroy, OnInit } from "@angular/core";
import { MatSnackBar } from "@angular/material/snack-bar";
import { Observable, Subscription } from "rxjs";
import { BridgeService } from "../../../services/bridge.service";
import { BridgeResponseDTO } from "../../../../dto/bridgeDTO";

@Component({
  selector: 'app-bridge-list',
  templateUrl: './bridge-list.component.html',
  styleUrls: ['./bridge-list.component.css']
})
export class BridgeListComponent implements OnInit, OnDestroy {

  bridgeServiceSubscription$ = new Subscription();
  dataSource: BridgeResponseDTO[] = [];

  columnsToDisplay = ['code', 'name', 'floorANumber', 'buildingAName', 'floorBNumber', 'buildingBName'];

  constructor(
              private bridgeService: BridgeService,
              private _snackBar: MatSnackBar) {
  }


  ngOnInit(): void {
    this.bridgeServiceSubscription$ = this.bridgeService.getAllBridges().subscribe(
      bridgeData => {
        this.dataSource = bridgeData;
      },
    );
  }

  ngOnDestroy(): void {
    this.bridgeServiceSubscription$.unsubscribe();
  }

}
