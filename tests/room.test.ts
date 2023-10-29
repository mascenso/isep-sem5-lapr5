import { expect } from 'chai';
import {RoomType} from "../src/domain/roomType";
import {Room} from "../src/domain/room";

describe('Room', () => {
  it('given valid properties create valid Room instance', () => {
    const validProps = {
      buildingId: 'B1',
      floorId: 'F1',
      name: 'B306',
      roomType: RoomType.OFFICE,
      description: 'Sala muito linda',
    };

    const room = Room.create(validProps);

    expect(room.isSuccess).to.be.true;
    expect(room.getValue()).to.be.an.instanceOf(Room);
  });

  it('given properties without floorId fails to create Room instance', () => {
    const invalidProps = {
      buildingId: 'B3',
      floorId: undefined,
      name: 'A200',
      roomType: RoomType.AUDITORIUM,
      description: 'Auditorio ainda mais lindo',
    };

    const room = Room.create(invalidProps);

    expect(room.isFailure).to.be.true;
    expect(room.error).to.be.equal('floorId is null or undefined');
  });
});
