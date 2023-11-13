import {Component, OnDestroy, OnInit} from '@angular/core';
import {FormControl} from "@angular/forms";
import {animate, state, style, transition, trigger} from "@angular/animations";
import {BridgeDto, BridgeService} from "../../../services/bridge.service";
import {MatSnackBar} from "@angular/material/snack-bar";
import {Observable, Subscription} from "rxjs";
import { FloorDto, FloorService } from "../../../services/floor.service";

@Component({
  selector: 'app-bridge-list',
  templateUrl: './bridge-list.component.html',
  animations: [
    trigger('detailExpand', [
      state('collapsed,void', style({height: '0px', minHeight: '0'})),
      state('expanded', style({height: '*'})),
      transition('expanded <=> collapsed', animate('225ms cubic-bezier(0.4, 0.0, 0.2, 1)')),
    ]),
  ],
  styleUrls: ['./bridge-list.component.css']
})
export class BridgeListComponent implements OnInit, OnDestroy {

  floorList: FloorDto[] = [];
  floorSelectionControl =  new FormControl();
  floorServiceSubscription$ = new Subscription();

  bridgeServiceSubscription$ = new Subscription();

  dataSource: BridgeDto[] = [];
  columnsToDisplay = ['id', 'name', 'floorAId', 'floorBId'];
  columnsToDisplayWithExpand = [...this.columnsToDisplay, 'expand'];
  expandedElement: BridgeDto | null | undefined;

  constructor(private floorService: FloorService,
              private bridgeService: BridgeService,
              private _snackBar: MatSnackBar) {}

  ngOnInit(): void {
    // fetch floor list from service
    this.bridgeServiceSubscription$ = this.bridgeService.getAllBridges().subscribe(
      bridgeData => {
        this.dataSource = bridgeData;
      },
      error => {
        this._snackBar.open("Unable to get bridges!", "close", {
          duration: 5000,
          panelClass: ['snackbar-warning']
        });
      }
    )
  }

  onSelectionUpdateTable(selection: any): void {

  }

  ngOnDestroy(): void {
    this.floorServiceSubscription$.unsubscribe();
    this.bridgeServiceSubscription$.unsubscribe();
  }

}
