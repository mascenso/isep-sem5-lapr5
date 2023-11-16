import {Component, OnDestroy, OnInit} from '@angular/core';
import {FormControl} from "@angular/forms";
import {BuildingResponseDto, BuildingService} from "../../../services/building.service";
import {animate, state, style, transition, trigger} from "@angular/animations";
import {FloorDto, FloorService} from "../../../services/floor.service";
import {MatSnackBar} from "@angular/material/snack-bar";
import {Observable, Subscription} from "rxjs";

@Component({
  selector: 'app-floor-list',
  templateUrl: './floor-list.component.html',
  animations: [
    trigger('detailExpand', [
      state('collapsed,void', style({height: '0px', minHeight: '0'})),
      state('expanded', style({height: '*'})),
      transition('expanded <=> collapsed', animate('225ms cubic-bezier(0.4, 0.0, 0.2, 1)')),
    ]),
  ],
  styleUrls: ['./floor-list.component.css']
})
export class FloorListComponent implements OnInit, OnDestroy {

  buildingList: BuildingResponseDto[] = [];
  buildingSelectionControl =  new FormControl();
  buildingServiceSubscription$ = new Subscription();

  floorServiceSubscription$ = new Subscription();

  dataSource: FloorDto[] = [];
  columnsToDisplay = ['id', 'floorNumber', 'width', 'length'];
  columnsToDisplayWithExpand = [...this.columnsToDisplay, 'expand'];
  expandedElement: FloorDto | null | undefined;

  constructor(private buildingService: BuildingService,
              private floorService: FloorService,
              private _snackBar: MatSnackBar) {}

  ngOnInit(): void {
    // fetch building list from service
    this.buildingServiceSubscription$ = this.buildingService.getAllBuildings().subscribe(
      response => {
        this.buildingList = response;
      },
      error => {
        this._snackBar.open("Unable to get buildings!", "close", {
          duration: 5000,
          panelClass: ['snackbar-warning']
        });
      }
    )
  }

  onSelectionUpdateTable(selection: any): void {
    console.log(selection);
    if (selection) {
      this.floorServiceSubscription$ = this.floorService.getFloorsWithElevatorByBuildingId(selection, true ).subscribe(
        floorData => {
          this.dataSource = floorData;
        },
        error => {
          this._snackBar.open(error.error, "close", {
            duration: 5000,
            panelClass: ['snackbar-error']
          });
        }
      )
    }
  }

  ngOnDestroy(): void {
    this.buildingServiceSubscription$.unsubscribe();
    this.floorServiceSubscription$.unsubscribe();
  }

}
