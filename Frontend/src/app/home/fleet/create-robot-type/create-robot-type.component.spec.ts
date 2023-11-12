import { ComponentFixture, TestBed } from '@angular/core/testing';

import { CreateRobotTypeComponent } from './create-robot-type.component';

describe('CreateRobotTypeComponent', () => {
  let component: CreateRobotTypeComponent;
  let fixture: ComponentFixture<CreateRobotTypeComponent>;

  beforeEach(() => {
    TestBed.configureTestingModule({
      declarations: [CreateRobotTypeComponent]
    });
    fixture = TestBed.createComponent(CreateRobotTypeComponent);
    component = fixture.componentInstance;
    fixture.detectChanges();
  });

  it('should create', () => {
    expect(component).toBeTruthy();
  });
});
