import { AfterViewInit, Component, ViewChild } from '@angular/core';
import { MatPaginator } from "@angular/material/paginator";
import { MatSort } from "@angular/material/sort";
import { Subscription } from "rxjs";
import { BridgeService } from "../../services/bridge.service";

@Component({
  selector: 'app-campus',
  templateUrl: './campus.component.html',
  styleUrls: ['./campus.component.css']
})
export class CampusComponent implements AfterViewInit {

  bridgeServiceSubscription$ = new Subscription();

  @ViewChild(MatPaginator) paginator: MatPaginator | undefined;
  @ViewChild(MatSort) sort: MatSort | undefined;
  canActivateBridge: boolean = true;

  constructor(private bridgeService: BridgeService) {
  }

  ngAfterViewInit(): void {

  }




}
