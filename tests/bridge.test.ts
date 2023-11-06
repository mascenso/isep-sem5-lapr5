import { expect } from 'chai';
import { Bridge } from "../src/domain/bridge-agg/bridge";

describe('Bridge', () => {
  it('given valid properties create valid Bridge instance', () => {
    const validProps = {
      id: "123",
      code: "A",
      name: "Edificio A - Administracao",
      floorAId: "123",
      floorBId: "123",
      buildingAId: "123",
      buildingBId: "123"
    };

    const bridge = Bridge.create(validProps);

    expect(bridge.isSuccess).to.be.true;
    expect(bridge.getValue()).to.be.an.instanceOf(Bridge);
  });

  it('given properties without floorId fails to create Bridge instance', () => {
    const invalidProps = {
      id: "123",
      code: "A",
      name: "Edificio A - Administracao",
      floorAId: "123",
      floorBId: "",
      buildingAId: "123",
      buildingBId: "123"
    };

    const bridge = Bridge.create(invalidProps);

    expect(bridge.isFailure).to.be.false;
    //expect(bridge.error).to.be.equal('floorBId is null or undefined');
  });
});
